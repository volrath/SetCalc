import Lexer
import Test.HUnit
import Control.Exception
import Data.Char

-- Test tokens simples

testSingleTokens = TestList [
  TestLabel "Tokens simples" $
  TestList [
    TestCase $ assertEqual "Token 'de'"
                           (lexer "de")
                           [TkDe (1,1)],
    TestCase $ assertEqual "Token 'digito'"
                           (lexer "digito")
                           [TkDigito (1,1)],
    TestCase $ assertEqual "Token 'dominio'"
                           (lexer "dominio")
                           [TkDominio (1,1)],
    TestCase $ assertEqual "Token 'es'"
                           (lexer "es")
                           [TkEs (1,1)],
    TestCase $ assertEqual "Token 'estado'"
                           (lexer "estado")
                           [TkEstado (1,1)],
    TestCase $ assertEqual "Token 'fin'"
                           (lexer "fin")
                           [TkFin (1,1)],
    TestCase $ assertEqual "Token 'letra'"
                           (lexer "letra")
                           [TkLetra (1,1)],
    TestCase $ assertEqual "Token 'mayuscula'"
                           (lexer "mayuscula")
                           [TkMayuscula (1,1)],
    TestCase $ assertEqual "Token 'miembro'"
                           (lexer "miembro")
                           [TkMiembro (1,1)],
    TestCase $ assertEqual "Token 'not'"
                           (lexer "not")
                           [TkNegar (1,1)],
    TestCase $ assertEqual "Token 'olvidar'"
                           (lexer "olvidar")
                           [TkOlvidar (1,1)],
    TestCase $ assertEqual "Token 'simbolo'"
                           (lexer "simbolo")
                           [TkSimbolo (1,1)],
    TestCase $ assertEqual "Token 'subconjunto'"
                           (lexer "subconjunto")
                           [TkSubconjunto (1,1)],
    TestCase $ assertEqual "Token 'tiene'"
                           (lexer "tiene")
                           [TkTiene (1,1)],
    TestCase $ assertEqual "Token 'todo'"
                           (lexer "todo")
                           [TkTodo (1,1)],
    TestCase $ assertEqual "Token 'universal'"
                           (lexer "universal")
                           [TkUniversal (1,1)],
    TestCase $ assertEqual "Token 'universo'"
                           (lexer "universo")
                           [TkUniverso (1,1)],
    TestCase $ assertEqual "Token 'vacio'"
                           (lexer "vacio")
                           [TkVacio (1,1)],
    TestCase $ assertEqual "Token '('"
                           (lexer "(")
                           [TkAParentesis (1,1)],
    TestCase $ assertEqual "Token ')'"
                           (lexer ")")
                           [TkCParentesis (1,1)],
    TestCase $ assertEqual "Token '{'"
                           (lexer "{")
                           [TkALlave (1,1)],
    TestCase $ assertEqual "Token '}'"
                           (lexer "}")
                           [TkCLlave (1,1)],
    TestCase $ assertEqual "Token '['"
                           (lexer "[")
                           [TkACorchete (1,1)],
    TestCase $ assertEqual "Token ']'"
                           (lexer "]")
                           [TkCCorchete (1,1)],
    TestCase $ assertEqual "Token '|'"
                           (lexer "|")
                           [TkBarra (1,1)],
    TestCase $ assertEqual "Token ','"
                           (lexer ",")
                           [TkComa (1,1)],
    TestCase $ assertEqual "Token '.'"
                           (lexer ".")
                           [TkPunto (1,1)],
    TestCase $ assertEqual "Token '..'"
                           (lexer "..")
                           [TkPuntoPunto (1,1)],
    TestCase $ assertEqual "Token '<-'"
                           (lexer "<-")
                           [TkFlecha (1,1)],
    TestCase $ assertEqual "Token ':='"
                           (lexer ":=")
                           [TkAsignacion (1,1)],
    TestCase $ assertEqual "Token '+'"
                           (lexer "+")
                           [TkUnion (1,1)],
    TestCase $ assertEqual "Token '*'"
                           (lexer "*")
                           [TkInterseccion (1,1)],
    TestCase $ assertEqual "Token '-'"
                           (lexer "-")
                           [TkDiferencia (1,1)],
    TestCase $ assertEqual "Token '~'"
                           (lexer "~")
                           [TkComplemento (1,1)],
    TestCase $ assertEqual "Token '%'"
                           (lexer "%")
                           [TkCartesiano (1,1)],
    TestCase $ assertEqual "Token '!'"
                           (lexer "!")
                           [TkPartes (1,1)],
    TestCase $ assertEqual "Token '=='"
                           (lexer "==")
                           [TkIgual (1,1)],
    TestCase $ assertEqual "Token '<'"
                           (lexer "<")
                           [TkMenor (1,1)],
    TestCase $ assertEqual "Token '>'"
                           (lexer ">")
                           [TkMayor (1,1)]        
    ]
  ]

-- Test tokens simples entre espacios

testSingleTokensBetweenSpaces = TestList [
  TestLabel "Tokens simples entre espacios" $
  TestList [
    TestCase $ assertEqual "Token 'de'"
                           (lexer "\n\n \n\t\n \t\n\t de\n\n \n\t\n \t\n\t ")
                           [TkDe (6,10)],
    TestCase $ assertEqual "Token 'digito'"
                           (lexer "\n\n \n\t\n \t\n\t digito\n\n \n\t\n \t\n\t ")
                           [TkDigito (6,10)],
    TestCase $ assertEqual "Token 'dominio'"
                           (lexer "\n\n \n\t\n \t\n\t dominio\n\n \n\t\n \t\n\t ")
                           [TkDominio (6,10)],
    TestCase $ assertEqual "Token 'es'"
                           (lexer "\n\n \n\t\n \t\n\t es\n\n \n\t\n \t\n\t ")
                           [TkEs (6,10)],
    TestCase $ assertEqual "Token 'estado'"
                           (lexer "\n\n \n\t\n \t\n\t estado\n\n \n\t\n \t\n\t ")
                           [TkEstado (6,10)],
    TestCase $ assertEqual "Token 'fin'"
                           (lexer "\n\n \n\t\n \t\n\t fin\n\n \n\t\n \t\n\t ")
                           [TkFin (6,10)],
    TestCase $ assertEqual "Token 'letra'"
                           (lexer "\n\n \n\t\n \t\n\t letra\n\n \n\t\n \t\n\t ")
                           [TkLetra (6,10)],
    TestCase $ assertEqual "Token 'mayuscula'"
                           (lexer "\n\n \n\t\n \t\n\t mayuscula\n\n \n\t\n \t\n\t ")
                           [TkMayuscula (6,10)],
    TestCase $ assertEqual "Token 'miembro'"
                           (lexer "\n\n \n\t\n \t\n\t miembro\n\n \n\t\n \t\n\t ")
                           [TkMiembro (6,10)],
    TestCase $ assertEqual "Token 'not'"
                           (lexer "\n\n \n\t\n \t\n\t not\n\n \n\t\n \t\n\t ")
                           [TkNegar (6,10)],
    TestCase $ assertEqual "Token 'olvidar'"
                           (lexer "\n\n \n\t\n \t\n\t olvidar\n\n \n\t\n \t\n\t ")
                           [TkOlvidar (6,10)],
    TestCase $ assertEqual "Token 'simbolo'"
                           (lexer "\n\n \n\t\n \t\n\t simbolo\n\n \n\t\n \t\n\t ")
                           [TkSimbolo (6,10)],
    TestCase $ assertEqual "Token 'subconjunto'"
                           (lexer "\n\n \n\t\n \t\n\t subconjunto\n\n \n\t\n \t\n\t ")
                           [TkSubconjunto (6,10)],
    TestCase $ assertEqual "Token 'tiene'"
                           (lexer "\n\n \n\t\n \t\n\t tiene\n\n \n\t\n \t\n\t ")
                           [TkTiene (6,10)],
    TestCase $ assertEqual "Token 'todo'"
                           (lexer "\n\n \n\t\n \t\n\t todo\n\n \n\t\n \t\n\t ")
                           [TkTodo (6,10)],
    TestCase $ assertEqual "Token 'universal'"
                           (lexer "\n\n \n\t\n \t\n\t universal\n\n \n\t\n \t\n\t ")
                           [TkUniversal (6,10)],
    TestCase $ assertEqual "Token 'universo'"
                           (lexer "\n\n \n\t\n \t\n\t universo\n\n \n\t\n \t\n\t ")
                           [TkUniverso (6,10)],
    TestCase $ assertEqual "Token 'vacio'"
                           (lexer "\n\n \n\t\n \t\n\t vacio\n\n \n\t\n \t\n\t ")
                           [TkVacio (6,10)],
    TestCase $ assertEqual "Token '('"
                           (lexer "\n\n \n\t\n \t\n\t (\n\n \n\t\n \t\n\t ")
                           [TkAParentesis (6,10)],
    TestCase $ assertEqual "Token ')'"
                           (lexer "\n\n \n\t\n \t\n\t )\n\n \n\t\n \t\n\t ")
                           [TkCParentesis (6,10)],
    TestCase $ assertEqual "Token '{'"
                           (lexer "\n\n \n\t\n \t\n\t {\n\n \n\t\n \t\n\t ")
                           [TkALlave (6,10)],
    TestCase $ assertEqual "Token '}'"
                           (lexer "\n\n \n\t\n \t\n\t }\n\n \n\t\n \t\n\t ")
                           [TkCLlave (6,10)],
    TestCase $ assertEqual "Token '['"
                           (lexer "\n\n \n\t\n \t\n\t [\n\n \n\t\n \t\n\t ")
                           [TkACorchete (6,10)],
    TestCase $ assertEqual "Token ']'"
                           (lexer "\n\n \n\t\n \t\n\t ]\n\n \n\t\n \t\n\t ")
                           [TkCCorchete (6,10)],
    TestCase $ assertEqual "Token '|'"
                           (lexer "\n\n \n\t\n \t\n\t |\n\n \n\t\n \t\n\t ")
                           [TkBarra (6,10)],
    TestCase $ assertEqual "Token ','"
                           (lexer "\n\n \n\t\n \t\n\t ,\n\n \n\t\n \t\n\t ")
                           [TkComa (6,10)],
    TestCase $ assertEqual "Token '.'"
                           (lexer "\n\n \n\t\n \t\n\t .\n\n \n\t\n \t\n\t ")
                           [TkPunto (6,10)],
    TestCase $ assertEqual "Token '..'"
                           (lexer "\n\n \n\t\n \t\n\t ..\n\n \n\t\n \t\n\t ")
                           [TkPuntoPunto (6,10)],
    TestCase $ assertEqual "Token '<-'"
                           (lexer "\n\n \n\t\n \t\n\t <-\n\n \n\t\n \t\n\t ")
                           [TkFlecha (6,10)],
    TestCase $ assertEqual "Token ':='"
                           (lexer "\n\n \n\t\n \t\n\t :=\n\n \n\t\n \t\n\t ")
                           [TkAsignacion (6,10)],
    TestCase $ assertEqual "Token '+'"
                           (lexer "\n\n \n\t\n \t\n\t +\n\n \n\t\n \t\n\t ")
                           [TkUnion (6,10)],
    TestCase $ assertEqual "Token '*'"
                           (lexer "\n\n \n\t\n \t\n\t *\n\n \n\t\n \t\n\t ")
                           [TkInterseccion (6,10)],
    TestCase $ assertEqual "Token '-'"
                           (lexer "\n\n \n\t\n \t\n\t -\n\n \n\t\n \t\n\t ")
                           [TkDiferencia (6,10)],
    TestCase $ assertEqual "Token '~'"
                           (lexer "\n\n \n\t\n \t\n\t ~\n\n \n\t\n \t\n\t ")
                           [TkComplemento (6,10)],
    TestCase $ assertEqual "Token '%'"
                           (lexer "\n\n \n\t\n \t\n\t %\n\n \n\t\n \t\n\t ")
                           [TkCartesiano (6,10)],
    TestCase $ assertEqual "Token '!'"
                           (lexer "\n\n \n\t\n \t\n\t !\n\n \n\t\n \t\n\t ")
                           [TkPartes (6,10)],
    TestCase $ assertEqual "Token '=='"
                           (lexer "\n\n \n\t\n \t\n\t ==\n\n \n\t\n \t\n\t ")
                           [TkIgual (6,10)],
    TestCase $ assertEqual "Token '<'"
                           (lexer "\n\n \n\t\n \t\n\t <\n\n \n\t\n \t\n\t ")
                           [TkMenor (6,10)],
    TestCase $ assertEqual "Token '>'"
                           (lexer "\n\n \n\t\n \t\n\t >\n\n \n\t\n \t\n\t ")
                           [TkMayor (6,10)]        
    ]
  ]

-- Test tokens simples en comentarios

testSingleTokensInComments = TestList [
  TestLabel "Tokens simples en comentarios" $
  TestList [
    TestCase $ assertEqual "Token 'de'"
                           (lexer "-- de ")
                           [],
    TestCase $ assertEqual "Token 'digito'"
                           (lexer "-- digito ")
                           [],
    TestCase $ assertEqual "Token 'dominio'"
                           (lexer "-- dominio ")
                           [],
    TestCase $ assertEqual "Token 'es'"
                           (lexer "-- es ")
                           [],
    TestCase $ assertEqual "Token 'estado'"
                           (lexer "-- estado ")
                           [],
    TestCase $ assertEqual "Token 'fin'"
                           (lexer "-- fin ")
                           [],
    TestCase $ assertEqual "Token 'letra'"
                           (lexer "-- letra ")
                           [],
    TestCase $ assertEqual "Token 'mayuscula'"
                           (lexer "-- mayuscula ")
                           [],
    TestCase $ assertEqual "Token 'miembro'"
                           (lexer "-- miembro ")
                           [],
    TestCase $ assertEqual "Token 'not'"
                           (lexer "-- not ")
                           [],
    TestCase $ assertEqual "Token 'olvidar'"
                           (lexer "-- olvidar ")
                           [],
    TestCase $ assertEqual "Token 'simbolo'"
                           (lexer "-- simbolo ")
                           [],
    TestCase $ assertEqual "Token 'subconjunto'"
                           (lexer "-- subconjunto ")
                           [],
    TestCase $ assertEqual "Token 'tiene'"
                           (lexer "-- tiene ")
                           [],
    TestCase $ assertEqual "Token 'todo'"
                           (lexer "-- todo ")
                           [],
    TestCase $ assertEqual "Token 'universal'"
                           (lexer "-- universal ")
                           [],
    TestCase $ assertEqual "Token 'universo'"
                           (lexer "-- universo ")
                           [],
    TestCase $ assertEqual "Token 'vacio'"
                           (lexer "-- vacio ")
                           [],
    TestCase $ assertEqual "Token '('"
                           (lexer "-- ( ")
                           [],
    TestCase $ assertEqual "Token ')'"
                           (lexer "-- ) ")
                           [],
    TestCase $ assertEqual "Token '{'"
                           (lexer "-- { ")
                           [],
    TestCase $ assertEqual "Token '}'"
                           (lexer "-- } ")
                           [],
    TestCase $ assertEqual "Token '['"
                           (lexer "-- [ ")
                           [],
    TestCase $ assertEqual "Token ']'"
                           (lexer "-- ] ")
                           [],
    TestCase $ assertEqual "Token '|'"
                           (lexer "-- | ")
                           [],
    TestCase $ assertEqual "Token ','"
                           (lexer "-- , ")
                           [],
    TestCase $ assertEqual "Token '.'"
                           (lexer "-- . ")
                           [],
    TestCase $ assertEqual "Token '..'"
                           (lexer "-- .. ")
                           [],
    TestCase $ assertEqual "Token '<-'"
                           (lexer "-- <- ")
                           [],
    TestCase $ assertEqual "Token ':='"
                           (lexer "-- := ")
                           [],
    TestCase $ assertEqual "Token '+'"
                           (lexer "-- + ")
                           [],
    TestCase $ assertEqual "Token '*'"
                           (lexer "-- * ")
                           [],
    TestCase $ assertEqual "Token '-'"
                           (lexer "-- - ")
                           [],
    TestCase $ assertEqual "Token '~'"
                           (lexer "-- ~ ")
                           [],
    TestCase $ assertEqual "Token '%'"
                           (lexer "-- % ")
                           [],
    TestCase $ assertEqual "Token '!'"
                           (lexer "-- ! ")
                           [],
    TestCase $ assertEqual "Token '=='"
                           (lexer "-- == ")
                           [],
    TestCase $ assertEqual "Token '<'"
                           (lexer "-- < ")
                           [],
    TestCase $ assertEqual "Token '>'"
                           (lexer "-- > ")
                           []        
    ]
  ]

-- Test ident token
--
testIdTokenCase = TestList [
  TestLabel "Token Identificador" $
  TestList [
    TestCase $ assertEqual "Token 'DE'"
                           (lexer "DE")
                           [TkId (1,1) "DE"],
    TestCase $ assertEqual "Token 'DIGITO'"
                           (lexer "DIGITO")
                           [TkId (1,1) "DIGITO"],
    TestCase $ assertEqual "Token 'DOMINIO'"
                           (lexer "DOMINIO")
                           [TkId (1,1) "DOMINIO"],
    TestCase $ assertEqual "Token 'ES'"
                           (lexer "ES")
                           [TkId (1,1) "ES"],
    TestCase $ assertEqual "Token 'ESTADO'"
                           (lexer "ESTADO")
                           [TkId (1,1) "ESTADO"],
    TestCase $ assertEqual "Token 'fin'"
                           (lexer "FIN")
                           [TkId (1,1) "FIN"],
    TestCase $ assertEqual "Token 'letra'"
                           (lexer "LETRA")
                           [TkId (1,1) "LETRA"],
    TestCase $ assertEqual "Token 'mayuscula'"
                           (lexer "MAYUSCULA")
                           [TkId (1,1) "MAYUSCULA"],
    TestCase $ assertEqual "Token 'miembro'"
                           (lexer "MIEMBRO")
                           [TkId (1,1) "MIEMBRO"],
    TestCase $ assertEqual "Token 'not'"
                           (lexer "NOT")
                           [TkId (1,1) "NOT"],
    TestCase $ assertEqual "Token 'olvidar'"
                           (lexer "OLVIDAR")
                           [TkId (1,1) "OLVIDAR"],
    TestCase $ assertEqual "Token 'simbolo'"
                           (lexer "SIMBOLO")
                           [TkId (1,1) "SIMBOLO"],
    TestCase $ assertEqual "Token 'subconjunto'"
                           (lexer "SUBCONJUNTO")
                           [TkId (1,1) "SUBCONJUNTO"],
    TestCase $ assertEqual "Token 'tiene'"
                           (lexer "TIENE")
                           [TkId (1,1) "TIENE"],
    TestCase $ assertEqual "Token 'todo'"
                           (lexer "TODO")
                           [TkId (1,1) "TODO"],
    TestCase $ assertEqual "Token 'universal'"
                           (lexer "UNIVERSAL")
                           [TkId (1,1) "UNIVERSAL"],
    TestCase $ assertEqual "Token 'universo'"
                           (lexer "UNIVERSO")
                           [TkId (1,1) "UNIVERSO"],
    TestCase $ assertEqual "Token 'vacio'"
                           (lexer "VACIO")
                           [TkId (1,1) "VACIO"]
    ]
  ]

-- Test String Token

testStringToken = TestList [
  TestLabel "Token String" $
  TestList [
    TestCase $ assertEqual "String vacio entre comillas simples"
                           (lexer "''")
                           [TkStr (1,1) ""],
    TestCase $ assertEqual "Token 'de' entre comillas simples"
                           (lexer "'de'")
                           [TkStr (1,1) "de"],
    TestCase $ assertEqual "Token 'digito' entre comillas simples"
                           (lexer "'digito'")
                           [TkStr (1,1) "digito"],
    TestCase $ assertEqual "Token 'dominio' entre comillas simples"
                           (lexer "'dominio'")
                           [TkStr (1,1) "dominio"],
    TestCase $ assertEqual "Token 'es' entre comillas simples"
                           (lexer "'es'")
                           [TkStr (1,1) "es"],
    TestCase $ assertEqual "Token 'estado' entre comillas simples"
                           (lexer "'estado'")
                           [TkStr (1,1) "estado"],
    TestCase $ assertEqual "Token 'fin' entre comillas simples"
                           (lexer "'fin'")
                           [TkStr (1,1) "fin"],
    TestCase $ assertEqual "Token 'letra' entre comillas simples"
                           (lexer "'letra'")
                           [TkStr (1,1) "letra"],
    TestCase $ assertEqual "Token 'mayuscula' entre comillas simples"
                           (lexer "'mayuscula'")
                           [TkStr (1,1) "mayuscula"],
    TestCase $ assertEqual "Token 'miembro' entre comillas simples"
                           (lexer "'miembro'")
                           [TkStr (1,1) "miembro"],
    TestCase $ assertEqual "Token 'not' entre comillas simples"
                           (lexer "'not'")
                           [TkStr (1,1) "not"],
    TestCase $ assertEqual "Token 'olvidar' entre comillas simples"
                           (lexer "'olvidar'")
                           [TkStr (1,1) "olvidar"],
    TestCase $ assertEqual "Token 'simbolo' entre comillas simples"
                           (lexer "'simbolo'")
                           [TkStr (1,1) "simbolo"],
    TestCase $ assertEqual "Token 'subconjunto' entre comillas simples"
                           (lexer "'subconjunto'")
                           [TkStr (1,1) "subconjunto"],
    TestCase $ assertEqual "Token 'tiene' entre comillas simples"
                           (lexer "'tiene'")
                           [TkStr (1,1) "tiene"],
    TestCase $ assertEqual "Token 'todo' entre comillas simples"
                           (lexer "'todo'")
                           [TkStr (1,1) "todo"],
    TestCase $ assertEqual "Token 'universal' entre comillas simples"
                           (lexer "'universal'")
                           [TkStr (1,1) "universal"],
    TestCase $ assertEqual "Token 'universo' entre comillas simples"
                           (lexer "'universo'")
                           [TkStr (1,1) "universo"],
    TestCase $ assertEqual "Token 'vacio' entre comillas simples"
                           (lexer "'vacio'")
                           [TkStr (1,1) "vacio"],
    TestCase $ assertEqual "Token '(' entre comillas simples"
                           (lexer "'('")
                           [TkStr (1,1) "("],
    TestCase $ assertEqual "Token ')' entre comillas simples"
                           (lexer "')'")
                           [TkStr (1,1) ")"],
    TestCase $ assertEqual "Token '{' entre comillas simples"
                           (lexer "'{'")
                           [TkStr (1,1) "{"],
    TestCase $ assertEqual "Token '}' entre comillas simples"
                           (lexer "'}'")
                           [TkStr (1,1) "}"],
    TestCase $ assertEqual "Token '[' entre comillas simples"
                           (lexer "'['")
                           [TkStr (1,1) "["],
    TestCase $ assertEqual "Token ']' entre comillas simples"
                           (lexer "']'")
                           [TkStr (1,1) "]"],
    TestCase $ assertEqual "Token '|' entre comillas simples"
                           (lexer "'|'")
                           [TkStr (1,1) "|"],
    TestCase $ assertEqual "Token ',' entre comillas simples"
                           (lexer "','")
                           [TkStr (1,1) ","],
    TestCase $ assertEqual "Token '.' entre comillas simples"
                           (lexer "'.'")
                           [TkStr (1,1) "."],
    TestCase $ assertEqual "Token '..' entre comillas simples"
                           (lexer "'..'")
                           [TkStr (1,1) ".."],
    TestCase $ assertEqual "Token '<-' entre comillas simples"
                           (lexer "'<-'")
                           [TkStr (1,1) "<-"],
    TestCase $ assertEqual "Token ':=' entre comillas simples"
                           (lexer "':='")
                           [TkStr (1,1) ":="],
    TestCase $ assertEqual "Token '+' entre comillas simples"
                           (lexer "'+'")
                           [TkStr (1,1) "+"],
    TestCase $ assertEqual "Token '*' entre comillas simples"
                           (lexer "'*'")
                           [TkStr (1,1) "*"],
    TestCase $ assertEqual "Token '-' entre comillas simples"
                           (lexer "'-'")
                           [TkStr (1,1) "-"],
    TestCase $ assertEqual "Token '~' entre comillas simples"
                           (lexer "'~'")
                           [TkStr (1,1) "~"],
    TestCase $ assertEqual "Token '%' entre comillas simples"
                           (lexer "'%'")
                           [TkStr (1,1) "%"],
    TestCase $ assertEqual "Token '!' entre comillas simples"
                           (lexer "'!'")
                           [TkStr (1,1) "!"],
    TestCase $ assertEqual "Token '==' entre comillas simples"
                           (lexer "'=='")
                           [TkStr (1,1) "=="],
    TestCase $ assertEqual "Token '<' entre comillas simples"
                           (lexer "'<'")
                           [TkStr (1,1) "<"],
    TestCase $ assertEqual "Token '>' entre comillas simples"
                           (lexer "'>'")
                           [TkStr (1,1) ">"],
    TestCase $ assertEqual "String vacio entre comillas dobles"
                           (lexer "\"\"" )
                           [TkStr (1,1) ""],
    TestCase $ assertEqual "Token 'de' entre comillas dobles"
                           (lexer "\"de\"")
                           [TkStr (1,1) "de"],
    TestCase $ assertEqual "Token 'digito' entre comillas dobles"
                           (lexer "\"digito\"")
                           [TkStr (1,1) "digito"],
    TestCase $ assertEqual "Token 'dominio' entre comillas dobles"
                           (lexer "\"dominio\"")
                           [TkStr (1,1) "dominio"],
    TestCase $ assertEqual "Token 'es' entre comillas dobles"
                           (lexer "\"es\"")
                           [TkStr (1,1) "es"],
    TestCase $ assertEqual "Token 'estado' entre comillas dobles"
                           (lexer "\"estado\"")
                           [TkStr (1,1) "estado"],
    TestCase $ assertEqual "Token 'fin' entre comillas dobles"
                           (lexer "\"fin\"")
                           [TkStr (1,1) "fin"],
    TestCase $ assertEqual "Token 'letra' entre comillas dobles"
                           (lexer "\"letra\"")
                           [TkStr (1,1) "letra"],
    TestCase $ assertEqual "Token 'mayuscula' entre comillas dobles"
                           (lexer "\"mayuscula\"")
                           [TkStr (1,1) "mayuscula"],
    TestCase $ assertEqual "Token 'miembro' entre comillas dobles"
                           (lexer "\"miembro\"")
                           [TkStr (1,1) "miembro"],
    TestCase $ assertEqual "Token 'not' entre comillas dobles"
                           (lexer "\"not\"")
                           [TkStr (1,1) "not"],
    TestCase $ assertEqual "Token 'olvidar' entre comillas dobles"
                           (lexer "\"olvidar\"")
                           [TkStr (1,1) "olvidar"],
    TestCase $ assertEqual "Token 'simbolo' entre comillas dobles"
                           (lexer "\"simbolo\"")
                           [TkStr (1,1) "simbolo"],
    TestCase $ assertEqual "Token 'subconjunto' entre comillas dobles"
                           (lexer "\"subconjunto\"")
                           [TkStr (1,1) "subconjunto"],
    TestCase $ assertEqual "Token 'tiene' entre comillas dobles"
                           (lexer "\"tiene\"")
                           [TkStr (1,1) "tiene"],
    TestCase $ assertEqual "Token 'todo' entre comillas dobles"
                           (lexer "\"todo\"")
                           [TkStr (1,1) "todo"],
    TestCase $ assertEqual "Token 'universal' entre comillas dobles"
                           (lexer "\"universal\"")
                           [TkStr (1,1) "universal"],
    TestCase $ assertEqual "Token 'universo' entre comillas dobles"
                           (lexer "\"universo\"")
                           [TkStr (1,1) "universo"],
    TestCase $ assertEqual "Token 'vacio' entre comillas dobles"
                           (lexer "\"vacio\"")
                           [TkStr (1,1) "vacio"],
    TestCase $ assertEqual "Token '(' entre comillas dobles"
                           (lexer "\"(\"")
                           [TkStr (1,1) "("],
    TestCase $ assertEqual "Token ')' entre comillas dobles"
                           (lexer "\")\"")
                           [TkStr (1,1) ")"],
    TestCase $ assertEqual "Token '{' entre comillas dobles"
                           (lexer "\"{\"")
                           [TkStr (1,1) "{"],
    TestCase $ assertEqual "Token '}' entre comillas dobles"
                           (lexer "\"}\"")
                           [TkStr (1,1) "}"],
    TestCase $ assertEqual "Token '[' entre comillas dobles"
                           (lexer "\"[\"")
                           [TkStr (1,1) "["],
    TestCase $ assertEqual "Token ']' entre comillas dobles"
                           (lexer "\"]\"")
                           [TkStr (1,1) "]"],
    TestCase $ assertEqual "Token '|' entre comillas dobles"
                           (lexer "\"|\"")
                           [TkStr (1,1) "|"],
    TestCase $ assertEqual "Token ',' entre comillas dobles"
                           (lexer "\",\"")
                           [TkStr (1,1) ","],
    TestCase $ assertEqual "Token '.' entre comillas dobles"
                           (lexer "\".\"")
                           [TkStr (1,1) "."],
    TestCase $ assertEqual "Token '..' entre comillas dobles"
                           (lexer "\"..\"")
                           [TkStr (1,1) ".."],
    TestCase $ assertEqual "Token '<-' entre comillas dobles"
                           (lexer "\"<-\"")
                           [TkStr (1,1) "<-"],
    TestCase $ assertEqual "Token ':=' entre comillas dobles"
                           (lexer "\":=\"")
                           [TkStr (1,1) ":="],
    TestCase $ assertEqual "Token '+' entre comillas dobles"
                           (lexer "\"+\"")
                           [TkStr (1,1) "+"],
    TestCase $ assertEqual "Token '*' entre comillas dobles"
                           (lexer "\"*\"")
                           [TkStr (1,1) "*"],
    TestCase $ assertEqual "Token '-' entre comillas dobles"
                           (lexer "\"-\"")
                           [TkStr (1,1) "-"],
    TestCase $ assertEqual "Token '~' entre comillas dobles"
                           (lexer "\"~\"")
                           [TkStr (1,1) "~"],
    TestCase $ assertEqual "Token '%' entre comillas dobles"
                           (lexer "\"%\"")
                           [TkStr (1,1) "%"],
    TestCase $ assertEqual "Token '!' entre comillas dobles"
                           (lexer "\"!\"")
                           [TkStr (1,1) "!"],
    TestCase $ assertEqual "Token '==' entre comillas dobles"
                           (lexer "\"==\"")
                           [TkStr (1,1) "=="],
    TestCase $ assertEqual "Token '<' entre comillas dobles"
                           (lexer "\"<\"")
                           [TkStr (1,1) "<"],
    TestCase $ assertEqual "Token '>' entre comillas dobles"
                           (lexer "\">\"")
                           [TkStr (1,1) ">"],
    TestCase $ assertEqual "Una comilla doble entre comillas simples"
                           (lexer "'\"'")
                           [TkStr (1,1) "\""],
    TestCase $ assertEqual "Dos comillas dobles entre comillas simples"
                           (lexer "'\"\"'")
                           [TkStr (1,1) "\"\""],
    TestCase $ assertEqual "Dos comillas simples entre comillas dobles"
                           (lexer "\"''\"")
                           [TkStr (1,1) "''"]
    ]
  ]

-- Test de errores.
-- Todos los caracteres imprimibles que NO están siendo usados
-- deben ocasionar una excepcion controlada en el analizador
-- lexicográfico.

wrap c  = [c]

comenta c = "-- " ++ wrap c

checkError c s = do
  let ok = (s == ("\nCaracter inesperado (" ++ c : "), linea 1, columna 1.\n"))
  case ok of
    True  -> return ()
    False -> assertFailure "Mensaje invalido"
     
testTokenError c = TestCase $ do
  let performCall = do
      evaluate ( lexer $ wrap c )
      assertFailure $ "Acepta caracter invalido (" ++ c : ")."
  handleJust errorCalls (checkError c) performCall

testNoError c = TestCase $
  assertEqual "Caracter invalido en comentario no genera error"
              ( lexer $ comenta c )
              []

errorChars =
  let isUsed c = or $ map (c==) " (){}[]|,.<-:=+*~%!>'\""
      in
         filter (not.isUsed) $
         filter (not.isLetter) $
         filter isPrint [ '\000'..'\177' ]

testLexErrors = TestList [
    TestLabel "Caracteres invalidos deben generar errores lexicograficos" $
    TestList $ map testTokenError errorChars,
    TestLabel "Caracteres invalidos no generar errores en comentarios" $
    TestList $ map testNoError errorChars
  ]

easyTests = TestList [
    testSingleTokens,
    testSingleTokensBetweenSpaces,
    testSingleTokensInComments,
    testIdTokenCase
  ]

hardTests = TestList [
    testStringToken,
    testLexErrors
  ]

nota v = fromIntegral i + f ff
  where (i,ff) = properFraction v
        f x | x >= 0.75 = 0.75
            | x >= 0.5  = 0.5
            | x >= 0.25 = 0.25
            | x >= 0    = 0

corregir = do
  counts <- runTestTT easyTests
  let v = 1.00
      c = fromIntegral $ cases counts
      e = fromIntegral $ errors counts
      f = fromIntegral $ failures counts
      p = (c - e - 1.1 * f) / c
      n = nota (p * v)
  putStrLn $ "Pruebas Faciles:  " ++ show (p*100) ++ "%"
  counts' <- runTestTT hardTests
  let v' = 2.0
      c' = fromIntegral $ cases counts
      e' = fromIntegral $ errors counts
      f' = fromIntegral $ failures counts
      p' = (c' - 2 * e' - 1.5 * f') / c'
      n' = nota (p' * v')
  putStrLn $ "Pruebas Complejas: " ++ show (p'*100) ++ "%"
  putStrLn $ "Nota  " ++ show (n+n') ++ "/" ++ show (v+v')
