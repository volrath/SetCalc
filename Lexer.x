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

%wrapper "posn"

-- Macros
$letra = [a-z A-Z]
$digito = [0-9]
$notQuotes = ~[\'\"\n]

-- rules
tokens :-
       $white+                                           ; -- ^ Espacios en blanco
       "--".*                                            ; -- ^ Comentarios
       \" [$notQuotes\']* \" | \' [$notQuotes\"]* \'     { \p s -> TkStr (getShortPosn p) (tail $ init s) } -- ^ Literales alfanum�ricos
       -- Palabras clave
       es                                                { \p s -> TkEs (getShortPosn p) } -- ^ Palabra reservada @es@
       de                                                { \p s -> TkDe (getShortPosn p) } -- ^ Palabra reservada @de@
       tiene                                             { \p s -> TkTiene (getShortPosn p) } -- ^ Palabra reservada @tiene@
       dominio                                           { \p s -> TkDominio (getShortPosn p) } -- ^ Palabra reservada @dominio@
       conjunto                                          { \p s -> TkConjunto (getShortPosn p) } -- ^ Palabra reservada @conjunto@
       universo                                          { \p s -> TkUniverso (getShortPosn p) } -- ^ Palabra reservada @universo@
       universal                                         { \p s -> TkUniversal (getShortPosn p) } -- ^ Palabra reservada @universal@
       -- Simbolos y separadores
       \{                                                { \p s -> TkALlave (getShortPosn p) } -- ^ Llave abriendo (@{@).
       \}                                                { \p s -> TkCLlave (getShortPosn p) } -- ^ Llave cerrando (@}@).
       \[                                                { \p s -> TkACorchete (getShortPosn p) } -- ^ Corchete abriendo (@[@).
       \]                                                { \p s -> TkCCorchete (getShortPosn p) } -- ^ Corchete cerrando (@]@).
       \(                                                { \p s -> TkAParentesis (getShortPosn p) } -- ^ Par�ntesis abriendo (@(@).
       \)                                                { \p s -> TkCParentesis (getShortPosn p) } -- ^ Par�ntesis cerrando (@)@).
       \,                                                { \p s -> TkComa (getShortPosn p) } -- ^ S�mbolo coma (@,@).
       \.                                                { \p s -> TkPunto (getShortPosn p) } -- ^ S�mbolo punto (@.@).
       \.\.                                              { \p s -> TkPuntoPunto (getShortPosn p) } -- ^ S�mbolo rango (@..@).
       \|                                                { \p s -> TkBarra (getShortPosn p) } -- ^ S�mbolo Barra (@|@).
       \<\-                                              { \p s -> TkFlecha (getShortPosn p) } -- ^ S�mbolo Flecha (@->@).
       -- Operadores
       :=                                                { \p s -> TkAsignacion (getShortPosn p) } -- ^ Operador de asignaci�n (@:=@).
       \+                                                { \p s -> TkUnion (getShortPosn p) } -- ^ Operador de uni�n (@+@).
       \*                                                { \p s -> TkInterseccion (getShortPosn p) } -- ^ Operador de intersecci�n (@*@).
       \-                                                { \p s -> TkDiferencia (getShortPosn p) } -- ^ Operador de diferencia (@-@).
       \~                                                { \p s -> TkComplemento (getShortPosn p) } -- ^ Operador de complemento (@~@).
       \%                                                { \p s -> TkCartesiano (getShortPosn p) } -- ^ Operador de producto cartesiano (@%@).
       \!                                                { \p s -> TkPartes (getShortPosn p) } -- ^ Operador de conjunto de partes (@!@).
       -- Funciones predefinidas
       miembro                                           { \p s -> TkMiembro (getShortPosn p) } -- ^ Palabra reservada @miembro@
       vacio                                             { \p s -> TkVacio (getShortPosn p) } -- ^ Palabra reservada @vacio@
       subconjunto                                       { \p s -> TkSubconjunto (getShortPosn p) } -- ^ Palabra reservada @subconjunto@
       -- Instrucciones especiales
       estado                                            { \p s -> TkEstado (getShortPosn p) } -- ^ Palabra reservada @estado@
       olvidar                                           { \p s -> TkOlvidar (getShortPosn p) } -- ^ Palabra reservada @olvidar@
       todo                                              { \p s -> TkTodo (getShortPosn p) } -- ^ Palabra reservada @todo@
       fin                                               { \p s -> TkFin (getShortPosn p) } -- ^ Palabra reservada @fin@
       -- Filtros para conjunto
       ==                                                { \p s -> TkIgual (getShortPosn p) } -- ^ Operador de igualdad (@==@).
       \<                                                { \p s -> TkMenor (getShortPosn p) } -- ^ Operador de menor (@<@).
       \>                                                { \p s -> TkMayor (getShortPosn p) } -- ^ Operador de mayor (@>@).
       mayuscula                                         { \p s -> TkMayuscula (getShortPosn p) } -- ^ Operador may�scula (@mayuscula@).
       letra                                             { \p s -> TkLetra (getShortPosn p) } -- ^ Operador letra (@letra@).
       digito                                            { \p s -> TkDigito (getShortPosn p) } -- ^ Operador digito (@digito@).
       simbolo                                           { \p s -> TkSimbolo (getShortPosn p) } -- ^ Operador s�mbolo (@simbolo@).
       not                                               { \p s -> TkNegar (getShortPosn p) } -- ^ Operador de negaci�n (@not@).
       -- Identificadores de dominio y variable
       $letra [$letra $digito \_]*                       { \p s -> TkId (getShortPosn p) s } -- ^ Identificadores de dominio y variable

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
