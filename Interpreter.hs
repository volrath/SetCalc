module Interpreter (
-- * Función Principal.
  interpreter,
  chequeoEstructural
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

type TupParser = (Map.Map String Symbol, AST)

interpreter :: TupParser
            -> IO()
interpreter (map, ast) = print $ printOperations map ast

printOperations :: Map.Map String Symbol
                -> AST
                -> String
printOperations map (Expr e) = (show e) ++ " ==> " ++ (show $ calcularExpresion map e) ++ "\n"
printOperations map (Secuencia (e:[])) = printOperations map (Expr e)
printOperations map (Secuencia (e:es)) = (printOperations map (Expr e)) ++ (printOperations map (Secuencia es))

calcularExpresion :: Map.Map String Symbol
                  -> Expresion
                  -> SetC Elemento
calcularExpresion map (Union e1 e2) = SetC.unionSet (calcularExpresion map e1) (calcularExpresion map e2)
calcularExpresion map (Interseccion e1 e2) = SetC.intersectSet (calcularExpresion map e1) (calcularExpresion map e2)
calcularExpresion map (Diferencia e1 e2) = SetC.minusSet (calcularExpresion map e1) (calcularExpresion map e2)
--calcularExpresion map (Complemento e e) = SetC.complementSet (calcularExpresion e)
--calcularExpresion map (Cartesiano e e) = SetC.crossProduct (calcularExpresion e) (calcularExpresion e)
--calcularExpresion map (Partes e) = SetC.powerSet (calcularExpresion map e)
calcularExpresion map (OpUniverso u) = evalUniverso map u
calcularExpresion map (OpExtension ext) = evalExtension map ext
calcularExpresion map (OpConj (Conjunto c d)) = c
calcularExpresion map (OpId t) = takeSetC $ takeConj (map Map.! (takeStr t))
calcularExpresion map (Asignacion t e) = calcularExpresion map e


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



{-
chequeoEstructural recibe dos mapas y averigua si sus tipos son equivalentes
-}
chequeoEstructural :: Elemento
                   -> Elemento
                   -> Bool
chequeoEstructural (Cto c1) (Cto c2)
    | SetC.isEmpty c1 && SetC.isEmpty c2 = True
    | SetC.isEmpty c1 && tieneElementos c2 = True
    | tieneElementos c1 && SetC.isEmpty c2 = True
    | (not $ SetC.isEmpty c1) && (not $ SetC.isEmpty c2) = chequeoEstructural (SetC.takeType c1) (SetC.takeType c2)
    | otherwise = False
chequeoEstructural (Lista []) (Lista ((Elem x2):es2)) = True
chequeoEstructural (Lista ((Elem x1):es1)) (Lista []) = True
chequeoEstructural (Lista []) (Lista []) = True
chequeoEstructural (Lista (e1:es1)) (Lista (e2:es2)) = chequeoEstructural e1 e2
chequeoEstructural (Elem s1) (Elem s2) = True
chequeoEstructural _ _ = False


evalExtension :: Map.Map String Symbol
              -> Ext
              -> SetC Elemento
evalExtension map e = SetC.emptySet

evalUniverso :: Map.Map String Symbol
             -> Univ
             -> SetC Elemento
evalUniverso map (UniversoT (Conjunto cu d)) = cu
evalUniverso map (UniversoDe t) = takeSetC $ takeConj (map Map.! (takeStr t))

{-|
  Función que devuelve el string envuelto por un Token cuyo constructor
  sea TkStr o TkId.
-}
takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s

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

takeSetC :: Conjunto
         -> SetC Elemento
takeSetC (Conjunto sc d) = sc

tieneElementos :: SetC Elemento
               -> Bool
tieneElementos c = case SetC.takeType c of
                     (Elem x) -> True
                     _ -> False