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

{-|
  Función que recibe un SymTable y un AST el cual ya ha sido chequeado estática y dinámicamente,
  y se encarga de imprimir cuales han sido las  expresiones realizadas.
-}
interpreter :: TupParser -- ^ Tupla de SymTable y AST a imprimir
            -> IO() -- ^ Impresión.
interpreter tp@(map, ast) = C.catch(putStr $ printOperations map ast) fail
                             where fail e = exitWith ExitSuccess

{-|
  Recibe un SymTable y un AST y se encarga de representar de forma amigable para el usuario
  las expresiones descritas en el AST.
-}
printOperations :: SymTable -- ^ SymTable en donde se buscará la representación de las variables.
                -> AST -- ^ AST a analizar.
                -> String -- ^ Representación amigable de las expresiones.
printOperations map (Expr (Instruccion i)) = printInstruction map i
printOperations map (Expr e) = (show e) ++ " ==> " ++ (show $ calcularExpresion map e) ++ "\n"
printOperations map (Secuencia []) = ""
printOperations map (Secuencia (e:[])) = printOperations map (Expr e)
printOperations map (Secuencia (e:es)) = (printOperations map (Expr e)) ++ (printOperations map (Secuencia es))


{-|
  Recibe un SymTable y una instrucción y se encarga de representar de forma amigable para el
  usuario el resultado de la ejecución de dicha instruccion
-}
printInstruction :: SymTable -- ^ SymTable en donde se buscará el resultado de las instrucciones.
                 -> Inst -- ^ Instrucción a ejecutar.
                 -> String -- ^ Representación de la ejecución de la instruccion
printInstruction map Estado = printMap map
printInstruction map Fin = error $ show Fin
printInstruction map i = show i

{-|
  Imprime el resultado de ejecutar la instrucción estado en un momento dado.
-}
printMap :: SymTable -- ^ SymTable de donde se sacarán todas las variables usadas y sus respectivos valores
         -> String -- ^ Representación.
printMap map = (printDomains map) ++ "\n" ++ (printSets map) ++ "\n"

{-|
  Imprime todos los dominios en un SymTable dado.
-}
printDomains :: SymTable -- ^ SymTable donde se buscarán los dominios.
             -> String -- ^ Representación de los dominios.
printDomains map = foldl (++) "Dominios:\n" (stringDomain map $ Map.toList map)

stringDomain :: SymTable
             -> [(String, Symbol)]
             -> [String]
stringDomain _ [] = []
stringDomain mapa ((var,sym):sts) = case dom of
                                      Just (DominioID d)  -> (var ++ " es dominio: " ++ (show $ takeStr d) ++ " ==> " ++ (show $ dominioSetC mapa (DominioID d)) ++ "\n"):(stringDomain mapa sts)
                                      Just (Dominio dset) -> (var ++ " es dominio: " ++ (show dset) ++ "\n"):(stringDomain mapa sts)
                                      Nothing -> stringDomain mapa sts
    where
      dom = (\(Symbol (d,c)) -> d) sym

{-|
  Imprime todos los conjuntos en un SymTable dado.
-}
printSets :: SymTable -- ^ SymTable donde se buscarán los conjuntos.
          -> String -- ^ Representación de los conjuntos.
printSets map = foldl (++) "Conjuntos:\n" (stringSet $ Map.toList map)

stringSet :: [(String, Symbol)]
          -> [String]
stringSet [] = []
stringSet ((var,sym):sts) = case set of
                              Just c@(Conjunto sc d) -> (var ++ " tiene dominio: " ++ (show d) ++ " y su valor actual es: " ++ (show c) ++ "\n"):(stringSet sts)
                              Nothing -> stringSet sts
    where
      set = (\(Symbol (d,c)) -> c) sym



{-|
  Recibe una tupla de SymTable y AST y devuelve una tupla con los posibles errores
  dinámicos encontrados en las asignaciones del AST.
-}
chequeoDinamico :: TupParser
                -> (String, SymTable)
chequeoDinamico (mapa, (Secuencia exprs)) = foldl chequeoDinamico' ([], mapa) exprs


chequeoDinamico' :: (String, SymTable)
                 -> Expresion
                 -> (String, SymTable)
chequeoDinamico' mapa (Union e1 e2) = mezclarMapas (mezclarMapas (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1)) mapa
chequeoDinamico' mapa (Interseccion e1 e2) = mezclarMapas (mezclarMapas (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1)) mapa
chequeoDinamico' mapa (Diferencia e1 e2) = mezclarMapas (mezclarMapas (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1)) mapa
chequeoDinamico' mapa (Cartesiano e1 e2) = mezclarMapas (mezclarMapas (chequeoDinamico' mapa e2) (chequeoDinamico' mapa e1)) mapa
chequeoDinamico' mapa (Complemento e) = chequeoDinamico' mapa e
chequeoDinamico' mapa (Partes e) = chequeoDinamico' mapa e
chequeoDinamico' mapa (Asignacion var e) =  case compararDominio var e of
                                             True  -> (fst mapa, actualizarConjS (takeStr var) newVal (snd (chequeoDinamico' mapa e)))
                                             False -> mezclarMapas mapa (showErr var e, snd mapa)
    where
      dominioDe var = dominioSetC (snd mapa) $ conjuntoDom $ takeConj ((snd mapa) Map.! (takeStr var))
      newVal = calcularExpresion (snd mapa) (Asignacion var e)
      showErr var e = "El resultado de la expresion " ++ (show e) ++ " no es compatible con el dominio de la variable " ++ (takeStr var) ++ " - linea: " ++ (show $ fst $ takePos var) ++ ", columna: " ++ (show $ snd $ takePos var) ++ "\n"
      compararDominio var (OpId t) = (takeStr var) == (takeStr t)
      compararDominio var _ = SetC.subSet newVal (dominioDe var)
chequeoDinamico' mapa (Instruccion e) = case e of
                                          Estado -> mapa
                                          Olvidar ids -> mapa
                                          OlvidarTodo -> ([], Map.empty)
                                          Fin -> ([], Map.empty)
chequeoDinamico' mapa _ = mapa

mezclarMapas (errs1, map1) (errs2, map2) = (errs1 ++ errs2, Map.union map1 map2)




calcularExpresion :: SymTable
                  -> Expresion
                  -> SetC Elemento
calcularExpresion map1 (Union e1 e2) = SetC.unionSet (calcularExpresion map1 e1) (calcularExpresion map1 e2)
calcularExpresion map1 (Interseccion e1 e2) = SetC.intersectSet (calcularExpresion map1 e1) (calcularExpresion map1 e2)
calcularExpresion map1 (Diferencia e1 e2) = SetC.minusSet (calcularExpresion map1 e1) (calcularExpresion map1 e2)
calcularExpresion map1 (Complemento e) =  evalComplemento map1 e
calcularExpresion map1 (Cartesiano e1 e2) = crossProduct (calcularExpresion map1 e1) (calcularExpresion map1 e2)
calcularExpresion map1 (Partes e) = SetC.mapSet Cto (SetC.powerSet (calcularExpresion map1 e))
calcularExpresion map1 (OpUniverso u) = evalUniverso map1 u
calcularExpresion map1 (OpExtension ext) = evalExtension map1 ext
calcularExpresion map1 (OpConj (Conjunto c d)) = c
calcularExpresion map1 (OpId t) = conjuntoSetC $ takeConj (map1 Map.! (takeStr t))
calcularExpresion map1 (Asignacion t e) = calcularExpresion map1 e
calcularExpresion map1 _  = SetC.emptySet

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
verificarTipos exp@(OpUniverso uni) = Nothing
verificarTipos exp@(OpExtension ext) = Nothing -- ^ Arreglar esto
verificarTipos exp@(OpConj c) = verificarTipoConjunto set set
    where set = conjuntoSetC c
verificarTipos exp@(OpId t) = Nothing
verificarTipos exp@(Asignacion var e) = case verificarTipos e of
                                          Nothing -> Nothing
                                          Just errs -> Just $ mostrarError exp e errs
verificarTipos exp@(Instruccion a) = Nothing

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
                                                         Tupla (a,b) -> case compararTipos a b of
                                                                          True -> Nothing
                                                                          False -> Just $ "\n  El tipo de datos del elemento " ++ (show intocable) ++ " esta mal definido"
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
compararTipos (Tupla (e1,es1)) (Tupla (e2,es2)) = compararTipos e1 e2
compararTipos (Elem s1) (Elem s2) = True
compararTipos _ _ = False


evalExtension :: SymTable
              -> Ext
              -> SetC Elemento
evalExtension mapa (ConjuntoExt (Ident el) ((Gen _ t):[]) fls) = SetC.fromList [x | x <- (generador mapa t), evalFiltros1 mapa x fls]
    where
      generador mapa t = (\(Just l) -> l) $ SetC.takeType $ conjuntoSetC $ takeConj $ (mapa Map.! (takeStr t))
evalExtension mapa (ConjuntoExt (Tupla (Ident x, Ident y)) [(Gen a t1), (Gen b t2)] fls)
    | (takeStr x) == a = SetC.fromList [ Tupla (x,y) | x <- (generador mapa t1), y <- (generador mapa t2)]
    | otherwise = SetC.fromList [ Tupla (x,y) | x <- (generador mapa t2), y <- (generador mapa t1)]
    where generador mapa t = (\(Just l) -> l) $ SetC.takeType $ conjuntoSetC $ takeConj $ (mapa Map.! (takeStr t))

evalFiltros1 :: SymTable
             -> Elemento
             -> [Filtro]
             -> Bool
evalFiltros1 mapa x fls = foldl (&&) True (map (filtrar1 mapa x) fls)


filtrar1 :: SymTable
         -> Elemento
         -> Filtro
         -> Bool
filtrar1 mapa (Elem s) (FilMayuscula _) = SetC.subSet (SetC.fromList s) (SetC.fromList ['A' .. 'Z'])
filtrar1 mapa (Elem s) (FilLetra _) = SetC.subSet (SetC.fromList s) (SetC.fromList (['A' .. 'Z'] ++ ['a' .. 'z']))
filtrar1 mapa (Elem s) (FilDigito _) = SetC.subSet (SetC.fromList s) (SetC.fromList ['0' .. '9'])
filtrar1 mapa (Elem s) (FilSimbolo _) = SetC.subSet (SetC.fromList s) (SetC.fromList (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']))
filtrar1 mapa (Cto a) (FilIgual _ (Ident t)) = (conjuntoDe mapa t) == a
    where
      conjuntoDe mapa t = conjuntoSetC $ takeConj (mapa Map.! (takeStr t))
filtrar1 mapa a (FilIgual _ b) = a == b
filtrar1 mapa (Elem a) (FilMenor _ (Elem b)) = a < b
filtrar1 mapa (Elem a) (FilMayor _ (Elem b)) = a > b
filtrar1 mapa _ _ = False









-- evalFiltros :: SymTable
--              -> [Filtro]
--              -> Bool
-- evalFiltros mapa fls = foldl (&&) True $ map (aplicarFiltro mapa) fls

-- aplicarFiltro :: SymTable
--               ->
--               -> Filtro
--               -> Bool
-- aplicarFiltro mapa (FilIgual e1 e2) = 
-- aplicarFiltro mapa (FilMenor e1 e2) =
-- aplicarFiltro mapa (FilMayor e1 e2) =
-- aplicarFiltro mapa (FilMayuscula e) = SetC.subset

        


{-
Evalua tanto un universo de una variable como el universo de todos
los caracteres imprimible en haskell.
-}
evalUniverso :: SymTable
             -> Univ
             -> SetC Elemento
evalUniverso map (UniversoT (Conjunto cu d)) = cu
evalUniverso map (UniversoDe t) = dominioSetC map $ takeDom (map Map.! (takeStr t))


evalComplemento :: SymTable
                -> Expresion
                -> SetC Elemento
evalComplemento mapa (OpId t) = SetC.minusSet (dominioDe mapa t) (conjuntoActualDe mapa t)
    where
      dominioDe mapa var = dominioSetC mapa $ conjuntoDom $ takeConj (mapa Map.! (takeStr var))
      conjuntoActualDe mapa var = conjuntoSetC $ takeConj (mapa Map.! (takeStr var))
evalComplemento mapa (Asignacion var e) = SetC.minusSet (dominioDe mapa var) (calcularExpresion mapa e)
    where
      dominioDe mapa var = dominioSetC mapa $ conjuntoDom $ takeConj (mapa Map.! (takeStr var))
evalComplemento mapa _ = SetC.emptySet
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
    where sobreescribirConj (Symbol (_, Just (Conjunto ncs ncd))) (Symbol (od, oc)) = case oc of
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
dominioSetC mapa (DominioID dom) = dominioSetC mapa $ takeDom $ mapa Map.! (takeStr dom)


sonElementos :: [Elemento]
             -> Bool
sonElementos [] = True 
sonElementos ((Elem e): es) = True && sonElementos es
sonElementos _ = False

crossProduct :: SetC Elemento
             -> SetC Elemento
             -> SetC Elemento
crossProduct x y = SetC.fromList [(Tupla (a,b)) | a <- (SetC.toList x), b <- (SetC.toList y)]