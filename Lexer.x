{
{-|
  Primera aproximacion del Lexer.
-}
module Lexer (
              -- * Tipo abstracto
              Token(..),
              -- * Funcion a exportar
              lexer
) where
import System.IO
import System.Exit
}

%wrapper "posn" -- Intentar pasar a monad

-- Macros
$letra = [a-z A-Z]
$digito = [0-9]
$notQuotes = ~[\'\"\n]

-- rules
tokens :-
       $white+                                           ;
       "--".*                                            ;
       -- Literales alfanum�ricos
       \" [$notQuotes\']* \" | \' [$notQuotes\"]* \'     { \p s -> TkStr (getShortPosn p) (tail $ init s) }
       -- Palabras clave
       es                                                { \p s -> TkEs (getShortPosn p) }
       de                                                { \p s -> TkDe (getShortPosn p) }
       tiene                                             { \p s -> TkTiene (getShortPosn p) }
       dominio                                           { \p s -> TkDominio (getShortPosn p) }
       conjunto                                          { \p s -> TkConjunto (getShortPosn p) }
       universo                                          { \p s -> TkUniverso (getShortPosn p) }
       universal                                         { \p s -> TkUniversal (getShortPosn p) }
       -- Simbolos y separadores
       \{                                                { \p s -> TkALlave (getShortPosn p) }
       \}                                                { \p s -> TkCLlave (getShortPosn p) }
       \[                                                { \p s -> TkACorchete (getShortPosn p) }
       \]                                                { \p s -> TkCCorchete (getShortPosn p) }
       \(                                                { \p s -> TkAParentesis (getShortPosn p) }
       \)                                                { \p s -> TkCParentesis (getShortPosn p) }
       \,                                                { \p s -> TkComa (getShortPosn p) }
       \.                                                { \p s -> TkPunto (getShortPosn p) }
       \.\.                                              { \p s -> TkPuntoPunto (getShortPosn p) }
       \|                                                { \p s -> TkBarra (getShortPosn p) }
       \<\-                                              { \p s -> TkFlecha (getShortPosn p) }
       -- Operadores
       \:=                                               { \p s -> TkAsignacion (getShortPosn p) }
       \+                                                { \p s -> TkUnion (getShortPosn p) }
       \*                                                { \p s -> TkInterseccion (getShortPosn p) }
       \-                                                { \p s -> TkDiferencia (getShortPosn p) }
       \~                                                { \p s -> TkComplemento (getShortPosn p) }
       \%                                                { \p s -> TkCartesiano (getShortPosn p) }
       \!                                                { \p s -> TkPartes (getShortPosn p) }
       -- Funciones predefinidas
       miembro                                           { \p s -> TkMiembro (getShortPosn p) }
       vacio                                             { \p s -> TkVacio (getShortPosn p) }
       subconjunto                                       { \p s -> TkSubconjunto (getShortPosn p) }
       -- Instrucciones especiales
       estado                                            { \p s -> TkEstado (getShortPosn p) }
       olvidar                                           { \p s -> TkOlvidar (getShortPosn p) }
       todo                                              { \p s -> TkTodo (getShortPosn p) }
       fin                                               { \p s -> TkFin (getShortPosn p) }
       -- Filtros para conjunto
       ==                                                { \p s -> TkIgual (getShortPosn p) }
       \<                                                { \p s -> TkMenor (getShortPosn p) }
       \>                                                { \p s -> TkMayor (getShortPosn p) }
       mayuscula                                         { \p s -> TkMayuscula (getShortPosn p) }
       letra                                             { \p s -> TkLetra (getShortPosn p) }
       digito                                            { \p s -> TkDigito (getShortPosn p) }
       simbolo                                           { \p s -> TkSimbolo (getShortPosn p) }
       not                                               { \p s -> TkNegar (getShortPosn p) }
       -- Identificadores de dominio y variable
       $letra [$letra $digito \_]*                       { \p s -> TkId (getShortPosn p) s }

{
{-|
  El tipo de datos @Token@ que contiene los /tokens/
  devueltos por el analizador lexicogr�fico generado.
  
  Es declarado derivando de @Show@ para poder probar
  individualmente el analizador lexicogr�fico.
  Tambi�n es declarado derivando de @Eq@ para ser
  usado por el analizador sint�ctico.
-}
data Token = TkEs (Int, Int)
           | TkDe (Int, Int)
           | TkTiene (Int, Int)
           | TkDominio (Int, Int)
           | TkConjunto (Int, Int)
           | TkUniverso (Int, Int)
           | TkUniversal (Int, Int)
           | TkStr (Int, Int) String
           | TkId (Int, Int) String
           | TkALlave (Int, Int)
           | TkCLlave (Int, Int)
           | TkACorchete (Int, Int)
           | TkCCorchete (Int, Int)
           | TkAParentesis (Int, Int)
           | TkCParentesis (Int, Int)
           | TkComa (Int, Int)
           | TkPunto (Int, Int)
           | TkPuntoPunto (Int, Int)
           | TkBarra (Int, Int)
           | TkFlecha (Int, Int)
           | TkAsignacion (Int, Int)
           | TkUnion (Int, Int)
           | TkInterseccion (Int, Int)
           | TkDiferencia (Int, Int)
           | TkComplemento (Int, Int)
           | TkCartesiano (Int, Int)
           | TkPartes (Int, Int)
           | TkMiembro (Int, Int)
           | TkVacio (Int, Int)
           | TkSubconjunto (Int, Int)
           | TkEstado (Int, Int)
           | TkOlvidar (Int, Int)
           | TkTodo (Int, Int)
           | TkFin (Int, Int)
           | TkIgual (Int, Int)
           | TkMenor (Int, Int)
           | TkMayor (Int, Int)
           | TkMayuscula (Int, Int)
           | TkLetra (Int, Int)
           | TkDigito (Int, Int)
           | TkSimbolo (Int, Int)
           | TkNegar (Int, Int)
           deriving (Eq, Show)

{-|
  lexer
  
  Funci�n que se apoya en el analizador
  lexicogr�fico y la funci�n @alexCatchingPosn@.
  recibe un @String@ y produce la lista de
  /tokens/ encontrados all�.
-}
lexer :: String -- ^ Cadena de caract�res a analizar
      -> [Token] -- ^ Lista de tokens resultante.
lexer s = catchErrors $ alexCatchingPosn s


{-|
  alexCatchingPosn
  
  Funci�n que emula el funcionamiento de alexScanTokens
  para conseguir la lista de /tokens/ encontrados en un
  @String@ pasado por par�metro y una lista de errores
  encontrados en el mismo.
-}
alexCatchingPosn :: String -- ^ Cadena de caract�res a analizar
                 -> (String, [Token]) -- ^ Tupla que representa los errores y /tokens/ encontrados
alexCatchingPosn str = go (alexStartPos, '\n', str)
  where go inp@(pos,c,str) =
          case alexScan inp 0 of
                AlexEOF     -> ("",[])
                AlexError inp' -> concatTup (go (createInput inp')) ("\nCaracter inesperado (" ++ (head str) : "), linea " ++ show (fst (getShortPosn pos)) ++ ", columna " ++ (show (snd (getShortPosn pos))) ++ ".\n", [])
                AlexSkip inp' _        -> concatTup (go inp') ("",[])
                AlexToken inp' len act -> concatTup ("", [(act pos (take len str))]) (go inp')

createInput :: AlexInput -> AlexInput
createInput ((AlexPn o l c), _, str) = ((AlexPn o l (c+1)), (head str), (tailOrNothing str))
    where tailOrNothing s = do
            if (null s)
              then ""
              else tail s

{-|
  concatTup
  
  Funci�n que concatena listas encapsuladas en tuplas.
-}
concatTup :: (String, [Token]) -- ^ Primera tupla
          -> (String, [Token]) -- ^ Segunda tupla
          -> (String, [Token]) -- ^ Tupla resultante
concatTup (x,y) (w,z) = (w++x, y++z)

{-|
  catchErrors
  
  Funci�n que consigue errores en las tuplas devueltas por
  @alexCatchingPosn@.
-}
catchErrors :: (String, [Token]) -- ^ Tupla de errores y tokens
            -> [Token] -- ^ Lista de /tokens/ resultante
catchErrors ("", x) = x
catchErrors (e, _) = error e

{-|
  getShortPosn
  
  Funci�n que dado un @AlexInput@, devuelve una tupla con
  la l�nea y columna que �ste almacena.
-}
getShortPosn :: AlexPosn -- ^ AlexInput
             -> (Int, Int) -- ^ Tupla de linea y columna
getShortPosn (AlexPn o l c) = (l, c)

}
