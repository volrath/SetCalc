module Interpreter (
-- * Función Principal.
  interpreter
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
--interpreter (C.IOException e) = C.throwIO e

printOperations :: Map.Map String Symbol
                -> AST
                -> String
printOperations map (Expr e) = (show e) ++ ": " ++ (show $ calcularExpresion map e) ++ "\n"
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