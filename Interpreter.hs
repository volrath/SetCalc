{-
  Los pasos son:
  Para el compilador:
  1. Parser, que devuelve un TupParser
  2. Hacer chequeoEstatico de TupParser
  
  Para el interpretador:
  1. Parser a una linea, devuelve el TupParser inocente de esa linea
  2. chequeoEstatico de (datamap actualizado union datamap de la linea entrante) y el
  ast de la linea entrante.
  3. Case del resultado de chequeoEstatico. Si dio un Left String entonces error a ese
  string, si dio Right un Mapa entonces ahora datamap actualizado es igual a ese mapa.
  4. repito..

Preguntas:
1. Las instrucciones especiales entran dentro de los que son los AST?
si no entran, entonces como hago para saber que fueron llamadas si no 
tengo como sacarlas de la funcion parser, debido a su firma

2. Basado en el primer punto 1.2 de la segunda pagina del enunciado y el primer punto de
la entrega de implementacion, El modulo interpreter debe tener todo el codigo para
realizar los chequeos dinamicos del lenguaje, entre los cuales esta que no se puede
asignar un valor incompatible a una variable, Entonces esto quiere decir que el mapa
actual no se debe actualizar con valores incompatibles, pero no puedo realizar
la actualizacion del mapa en el modulo Interpreter porque la unica funcion exportada
es interpreter cuya firma devuelve un IO, por lo tanto, no puedo devolver un
mapa actualizado con cualquier cosa que haya necesitado actualizarse (como las asignaciones)
despues de un chequeo de contexto dinamico
(Lee de nuevo el primer punto de Entrega de Implantacion, vas a revivir nuevas dudas)
-}

{-
-- Antes de cualquier tipo de chequeo dinamico hay que pasar por el estatico, el cual
consta de lo ya hecho, y ademas esta:
1. redefinir un dominio: se hace a nivel de parser, para capturar los dominios redefinidos
en una misma linea y tambien se hace a nivel de setcalc para capturar los dominios
redefinidos en lineas pasadas.
2. errores de tipos de datos en las operaciones: Esto es modificar chequearAsignacion
o crear una funcion similar, que recorra el AST en secuencias de expresiones y para
cada expreesion se fija en las operaciones binarias y compara los tipos de datos de las
dos expresiones de interes. Si todo va bien, entonces -------- y si va mal acumulo el
error y sigo.

-- El chequeo de tipos de una variable y una asignacion consta de lo siguiente:
1. que el resultado de la asignacion tenga un tipo con sentido, y no mixed up, algo
como { 1, 2, 3 } o { {1},{2},{3} } pero no {1, {2}, 3}. [ESTATICO, VA COMO PUNTO
1.5 EN LOS CHEQUEOS DE ARRIBA]
2. que los elementos del conjunto de asignacion sean estructuralmente parecidos al
dominio de la variable, es decir, que conjunto-asignacion sea un subset de
dominio-variable. [DINAMICO]
-}

{-
Recibe un mapa original y un ast, en el ast empieza a buscar asignaciones y cuando
las consigue, mete la variable del lado izquierdo como key de un nuevo mapa y guarda
en el lado derecho lo que devuelve calcularExpresion de dicha asignacion. Luego
con ese nuevo mapa, se contrastan si los valores de las variables tienen tipos aceptables
en el mapa original, segun el dominio ke tienen asignado
-}
--chequeoDinamico

module Interpreter (
-- * Función Principal.
  interpreter,
  chequeoEstructural,
  chequeoDinamico,
) where

import System.IO
import System.Exit
import Char
import qualified Control.Exception as C
import qualified Data.Map as Map
import SetC
import Lexer
import Parser
import Abstract

type SymTable = Map.Map String Symbol
type SetTable = Map.Map String (SetC Elemento)
type TupParser = (SymTable, AST)

interpreter :: TupParser
            -> IO()
interpreter tp@(map, ast) = case chequeoEstructural tp of
                              Nothing -> putStr $ printOperations map ast
                              Just errs -> error $ errs

printOperations :: SymTable
                -> AST
                -> String
printOperations map (Expr e) = (show e) ++ " ==> " ++ (show $ calcularExpresion map e) ++ "\n"
printOperations map (Secuencia []) = ""
printOperations map (Secuencia (e:[])) = printOperations map (Expr e)
printOperations map (Secuencia (e:es)) = (printOperations map (Expr e)) ++ (printOperations map (Secuencia es))


chequeoDinamico :: TupParser
                -> SymTable
chequeoDinamico (mapa, (Secuencia exprs)) = foldl chequeoDinamico' mapa exprs

chequeoDinamico' :: SymTable
                 -> Expresion
                 -> SymTable
chequeoDinamico' mapa (Union e1 e2) = Map.union mapa (Map.union (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1))
chequeoDinamico' mapa (Interseccion e1 e2) = Map.union mapa (Map.union (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1))
chequeoDinamico' mapa (Diferencia e1 e2) = Map.union mapa (Map.union (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1))
chequeoDinamico' mapa (Cartesiano e1 e2) = Map.union mapa (Map.union (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1))
chequeoDinamico' mapa (Complemento e) = chequeoDinamico' mapa e
chequeoDinamico' mapa (Partes e) = chequeoDinamico' mapa e
chequeoDinamico' mapa (Asignacion var e) =  case SetC.subSet newVal $ dominioDe var of
                                             True  -> actualizarConjS (takeStr var) newVal (chequeoDinamico' mapa e)
                                             False -> showErr var e
    where
      dominioDe var = dominioSetC mapa $ conjuntoDom $ takeConj (mapa Map.! (takeStr var))
      newVal = calcularExpresion mapa e
      showErr var e = error $ "El resultado de la expresion " ++ (show e) ++ " no es compatible con el dominio de la variable " ++ (takeStr var) ++ " - linea: " ++ (show $ fst $ takePos var) ++ ", columna: " ++ (show $ snd $ takePos var)
chequeoDinamico' mapa _ = mapa
-- chequeoDinamico' mapa (Asignacion var e) = case SetC.takeType $ newVal of
--                                             Nothing -> case SetC.takeType $ dominioDe var of
--                                                          Nothing -> Map.empty
--                                                          Just l(e:es) -> case sonElementos l of
--                                                                            True  -> Map.insert var newVal
--                                                                            False -> error $ showErr var e
--                                             Just l1@(e1:es1) -> case SetC.takeType $ dominioDe var of
--                                                                   Nothing -> error $ showErr var e
--                                                                   Just (e2:es2) -> case compararTipos e1 e2 of
--                                                                                      True ->




calcularExpresion :: SymTable
                  -> Expresion
                  -> SetC Elemento
calcularExpresion map1 (Union e1 e2) = SetC.unionSet (calcularExpresion map1 e1) (calcularExpresion map1 e2)
calcularExpresion map1 (Interseccion e1 e2) = SetC.intersectSet (calcularExpresion map1 e1) (calcularExpresion map1 e2)
calcularExpresion map1 (Diferencia e1 e2) = SetC.minusSet (calcularExpresion map1 e1) (calcularExpresion map1 e2)
--calcularExpresion map1 (Complemento e) = SetC.complementSet (calcularExpresion map1 e) 
calcularExpresion map1 (Cartesiano e1 e2) = SetC.mapSet Cto (SetC.crossProduct (calcularExpresion map1 e1) (calcularExpresion map1 e2))
calcularExpresion map1 (Partes e) = SetC.mapSet Cto (SetC.powerSet (calcularExpresion map1 e))
calcularExpresion map1 (OpUniverso u) = evalUniverso map1 u
calcularExpresion map1 (OpExtension ext) = evalExtension map1 ext
calcularExpresion map1 (OpConj (Conjunto c d)) = c
calcularExpresion map1 (OpId t) = conjuntoSetC $ takeConj (map1 Map.! (takeStr t))
calcularExpresion map1 (Asignacion t e) = calcularExpresion map1 e




-- OJO: recordar que chequeoEstructural se llama asi:
-- chequeoEstructural $ chequear mapaActual $ lexer line
-- sabiendo que chequear recibe el mapa actual, corre el parser
-- a la nueva linea, y si todo esta bien, entonces devuelve un TupParser
-- con el Mapa de las declaraciones viejas (del exActual) con las
-- declaraciones de la nueva linea y TENGO KE PEDIR que el AST de
-- ese TupParser resultado sea el AST de \'UNICAMENTE la nueva linea.
chequeoEstructural :: TupParser -- ^ El TupParser que tiene el mapa 
                   -> Maybe String -- ^ Errores... o no?
chequeoEstructural (mapa, (Secuencia exprs)) = chequeoEstructural' mapa exprs


-- Verifica los tipos de datos de todos los conjuntos escritos
-- literalmente en la expresion
chequeoEstructural' :: SymTable
                    -> [Expresion]
                    -> Maybe String
chequeoEstructural' mapa [] = Nothing
chequeoEstructural' mapa (e:es) = case verificarTipos e of
                                    Nothing -> case chequeoOperador mapa e of
                                                 Nothing -> chequeoEstructural' mapa es
                                                 Just errOp -> case chequeoEstructural' mapa es of
                                                                 Nothing -> Just errOp
                                                                 Just errs -> Just $ errOp ++ errs
                                    Just errTipoConj -> case chequeoEstructural' mapa es of
                                                          Nothing -> Just errTipoConj
                                                          Just errs -> Just $ errTipoConj ++ errs


-- Solo revisa si todos los conjuntos de una expresion estan bien
-- definidos, no revisa las operaciones ni sus operandos
verificarTipos :: Expresion
               -> Maybe String
verificarTipos exp@(Union e1 e2) = verificarTipos' exp e1 e2
verificarTipos exp@(Interseccion e1 e2) = verificarTipos' exp e1 e2
verificarTipos exp@(Diferencia e1 e2) = verificarTipos' exp e1 e2
verificarTipos exp@(Cartesiano e1 e2) = verificarTipos' exp e1 e2
verificarTipos exp@(Complemento e) = case verificarTipos e of
                                       Nothing -> Nothing
                                       Just errs -> Just $ mostrarError exp e errs
verificarTipos exp@(Partes e) = case verificarTipos e of
                                  Nothing -> Nothing
                                  Just errs -> Just $ mostrarError exp e errs
verificarTipos exp@(OpUniverso uni) = Nothing -- ^ Arreglar esto
verificarTipos exp@(OpExtension ext) = Nothing -- ^ Arreglar esto
verificarTipos exp@(OpConj c) = verificarTipoConjunto set set
    where set = conjuntoSetC c
verificarTipos exp@(OpId t) = Nothing
verificarTipos exp@(Asignacion var e) = case verificarTipos e of
                                          Nothing -> Nothing
                                          Just errs -> Just $ mostrarError exp e errs

verificarTipos' :: Expresion 
                -> Expresion
                -> Expresion
                -> Maybe String
verificarTipos' orig e1 e2 = case verificarTipos e1 of
                                     Nothing -> verificarTipos e2
                                     Just errs1 -> case verificarTipos e2 of
                                                     Nothing -> Just $ mostrarError orig e1 errs1
                                                     Just errs2 -> Just $ (mostrarError orig e1 errs1) ++ errs2

mostrarError :: Expresion -> Expresion -> String -> String
mostrarError orig op err = "\nError en el argumento de " ++ (show orig) ++ " llamado: " ++ (show op) ++ " => " ++ err ++ "\n"


chequeoOperador :: SymTable
                -> Expresion
                -> Maybe String
chequeoOperador mapa exp@(Union e1 e2) = chequeoOperador' mapa exp e1 e2
chequeoOperador mapa exp@(Interseccion e1 e2) = chequeoOperador' mapa exp e1 e2
chequeoOperador mapa exp@(Diferencia e1 e2) = chequeoOperador' mapa exp e1 e2
chequeoOperador mapa exp@(Cartesiano e1 e2) = chequeoOperador' mapa exp e1 e2
chequeoOperador mapa (Asignacion var e) = chequeoOperador mapa e
chequeoOperador _ _ = Nothing


chequeoOperador' :: SymTable
                 -> Expresion
                 -> Expresion
                 -> Expresion
                 -> Maybe String
chequeoOperador' mapa orig e1 e2 = case SetC.takeType $ calcularExpresion mapa e1 of
                                     Nothing -> case SetC.takeType $ calcularExpresion mapa e2 of
                                                  Nothing -> Nothing
                                                  Just l -> case sonElementos l of
                                                              True -> Nothing
                                                              False -> Just $ "Error en la operacion " ++ (show orig) ++ ":\n    Tipos incompatibles.\n\n"
                                     Just l1@(e1:es1) -> case SetC.takeType $ calcularExpresion mapa e2 of
                                                           Nothing -> case sonElementos l1 of
                                                                        True -> Nothing
                                                                        False -> Just $ "Error en la operacion " ++ (show orig) ++ ":\n    Tipos incompatibles.\n\n"
                                                           Just (e2:es2) -> case compararTipos e1 e2 of
                                                                              True -> Nothing
                                                                              False -> Just $ "Error en la operacion " ++ (show orig) ++ ":\n    Tipos incompatibles.\n\n"




verificarTipoConjunto :: SetC Elemento
                      -> SetC Elemento
                      -> Maybe String
verificarTipoConjunto set intocable = case SetC.takeType set of
                                        Nothing -> Nothing
                                        Just (x:[]) -> case x of
                                                         Elem s -> Nothing
                                                         Cto sc -> verificarTipoConjunto sc intocable
                                                         Lista l -> verificarTipoConjunto (SetC.fromList l) intocable
                                                         _ -> Nothing
                                        Just elem@(x1:(x2:[])) -> case compararTipos x1 x2 of
                                                                    True  -> Nothing
                                                                    False -> Just $ "\n  El tipo de datos del elemento " ++ (show intocable) ++ " esta mal definido"
                                        Just elem@(x1:(x2:xs)) -> case compararTipos x1 x2 of
                                                                    True  -> verificarTipoConjunto (SetC.fromList (x2:xs)) intocable
                                                                    False -> Just $ "\n  El tipo de datos del elemento " ++ (show intocable) ++ " esta mal definido"


{-
compararTipos recibe dos elementos y averigua si sus tipos son equivalentes
-}
compararTipos :: Elemento
              -> Elemento
              -> Bool
compararTipos (Cto c1) (Cto c2) = case SetC.takeType c1 of
                                    Nothing -> case SetC.takeType c2 of
                                                 Nothing -> True
                                                 Just elems -> sonElementos elems
                                    Just elems -> case SetC.takeType c2 of
                                                    Nothing -> sonElementos elems
                                                    Just elems2 -> compararTipos (head elems) (head elems2)
compararTipos (Lista []) (Lista ((Elem x2):es2)) = True
compararTipos (Lista ((Elem x1):es1)) (Lista []) = True
compararTipos (Lista []) (Lista []) = True
compararTipos (Lista (e1:es1)) (Lista (e2:es2)) = compararTipos e1 e2
compararTipos (Elem s1) (Elem s2) = True
compararTipos _ _ = False


evalExtension :: SymTable
              -> Ext
              -> SetC Elemento
evalExtension map e = SetC.emptySet


{-
Evalua tanto un universo de una variable como el universo de todos
los caracteres imprimible en haskell.
-}
evalUniverso :: SymTable
             -> Univ
             -> SetC Elemento
evalUniverso map (UniversoT (Conjunto cu d)) = cu
evalUniverso map (UniversoDe t) = dominioSetC map $ conjuntoDom$ takeConj (map Map.! (takeStr t))

{-
Recibe un
-}
sym2setTable :: SymTable
             -> SetTable
sym2setTable sym = toSetTable (Map.toList sym)
    where toSetTable ((key,(Symbol (_, Just (Conjunto sc d)))):[]) = Map.singleton key sc
          toSetTable ((key,(Symbol (_, Just (Conjunto sc d)))):es) = Map.union (Map.singleton key sc) (toSetTable es)
          toSetTable _ = Map.empty


{-
Recibe una variable que debe ser un conjunto en el symtable, y un setC a meter en el
lugar de esa variable, sin tocar el dominio del conjunto
-}
actualizarConjS :: String
                -> SetC Elemento
                -> SymTable
                -> SymTable
actualizarConjS var nconj m = Map.insertWith sobreescribirConj var (Symbol (Nothing, Just (Conjunto nconj (Dominio (SetC.emptySet))))) m
    where sobreescribirConj (Symbol (od, oc)) (Symbol (_, Just (Conjunto ncs ncd))) = case oc of
                                                         Nothing -> (Symbol (od, Just (Conjunto ncs ncd)))
                                                         Just (Conjunto sc d) -> (Symbol (od, Just (Conjunto ncs d)))

{-|
  Función que devuelve el string envuelto por un Token cuyo constructor
  sea TkStr o TkId.
-}
takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s

{-|
  Devuelve la posición ocupada por Tokens cuyo
  constructor sea TkStr o TkId
-}
takePos :: Token -> (Int,Int)
takePos (TkStr pos s) = pos
takePos (TkId pos s) = pos

{-|
  Devuelve el dominio asociado a un /Symbol/
-}
takeDom :: Symbol -- ^ /Symbol/ sobre el que se quiere su dominio asociado.
        -> Dominio -- ^ Dominio encontrado en el /Symbol/.
takeDom (Symbol (Just a, _)) = a
takeDom (Symbol (Nothing, _)) = error $ "Error 0x08042FFA"

{-|
  Análogamente a la funcion anterior, devuelve el conjunto asociado
  a un /Symbol/.
-}
takeConj :: Symbol  -- ^ /Symbol/ sobre el que se quiere su conjunto asociado.
         -> Conjunto -- ^ Conjunto encontrado en el /Symbol/.
takeConj (Symbol (_, Just a)) = a
takeConj (Symbol (_, Nothing)) = error $ "Error 0x08042FFD"

{-
  Obtiene el setC de un conjunto
-}
conjuntoSetC :: Conjunto
             -> SetC Elemento
conjuntoSetC (Conjunto sc d) = sc

{-
  Obtiene el dominio de un cojunto
-}
conjuntoDom :: Conjunto
            -> Dominio
conjuntoDom (Conjunto sc d) = d

{-
  Dado un symtable, busca el valor en setc de un dominio
-}
dominioSetC :: SymTable -> Dominio -> SetC Elemento
dominioSetC _ (Dominio sc) = sc
dominioSetC mapa (DominioID dom) = dominioSetC mapa $ takeDom $ mapa Map.! dom


sonElementos :: [Elemento]
             -> Bool
sonElementos [] = True 
sonElementos ((Elem e): es) = True && sonElementos es
sonElementos _ = False
