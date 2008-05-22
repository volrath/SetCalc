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
}

%wrapper "posn" -- Intentar pasar a monad

-- Macros
$letra = [a-z A-Z]
$digito = [0-9]

-- rules
tokens :-
       $white+                                           ;
       "--".*                                            ;
       -- Literales alfanuméricos
       \" $printable* \" | \' $printable* \'             { \p s -> TkStr (getShortPosn p) (tail $ init s) }
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
       .                                                 { \p s -> showError p s }
{
{-|
  Codigo final, se declara el tad y la funcion
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

lexer :: String -> [Token]
lexer = alexScanTokens

-- alexCatchingPosn str = go (alexStartPosn, '\n', str)
--   where go imp@(pos,_,str) =
--           case alexScan inp 0 of
--                 AlexEOF     -> []
--                 AlexError _ -> makeError "\nCaracter inesperado (), linea "
--                 AlexSkip inp' len     -> go inp'
--                 AlexToken inp len act -> act pos (take len str) : go inp'


getShortPosn :: AlexPosn -> (Int, Int)
getShortPosn (AlexPn o l c) = (l, c)

showError :: AlexPosn -> String -> Token
showError (AlexPn _ l c) car = error("\nCaracter inesperado (" ++ car ++ "), linea " ++ (show l) ++ ", columna " ++ (show c) ++ ".\n")
}
