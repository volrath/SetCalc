
{-|
  /Analizador Sintáctico y Tabla de Sí­mbolos para la Calculadora/


	Traductores e Interpretadores CI3725

        Grupo 9

	Integrantes:
        Daniel Barreto - 04-36723
        Kristoffer Pantic - 05-38675

	Este módulo, desarrollado en Happy, implanta un Analizador
	Sintáctico para el lenguaje SetCalc.

-}
module Parser (
	-- ** Analizador Sintáctico con Análisis de Contexto Estático.
	parser
) where

import Lexer
import SetC
import Abstract
import Char
import List
import qualified Data.Map as Map
}

%name elNoParser
%error { syntaxError }
%tokentype { Token }
%token
        es                       { TkEs a            }
        de                       { TkDe a            }
        tiene                    { TkTiene a         }
        dominio                  { TkDominio a       }
        universo                 { TkUniverso a      }
        universal                { TkUniversal a     }
        str                      { TkStr pos s       }
        id                       { TkId pos s        }
        '{'                      { TkALlave a        }
        '}'                      { TkCLlave a        }
        '['                      { TkACorchete a     }
        ']'                      { TkCCorchete a     }
        '('                      { TkAParentesis a   }
        ')'                      { TkCParentesis a   }
        ','                      { TkComa a          }
        '.'                      { TkPunto a         }
        '..'                     { TkPuntoPunto a    }
        '|'                      { TkBarra a         }
        '<-'                     { TkFlecha a        }
        ':='                     { TkAsignacion a    }
        '+'                      { TkUnion a         }
        '*'                      { TkInterseccion a  }
        '-'                      { TkDiferencia a    }
        '~'                      { TkComplemento a   }
        '%'                      { TkCartesiano a    }
        '!'                      { TkPartes a        }
        miembro                  { TkMiembro a       }
        vacio                    { TkVacio a         }
        subconjunto              { TkSubconjunto a   }
        estado                   { TkEstado a        }
        olvidar                  { TkOlvidar a       }
        todo                     { TkTodo a          }
        fin                      { TkFin a           }
        '=='                     { TkIgual a         }
        '<'                      { TkMenor a         }
        '>'                      { TkMayor a         }
        mayuscula                { TkMayuscula a     }
        letra                    { TkLetra a         }
        digito                   { TkDigito a        }
        simbolo                  { TkSimbolo a       }
        not                      { TkNegar a         }

%right ':='
%left '+' '-' '*' '%'
%nonassoc '~'
%nonassoc '!'
%left ','

%%

LProg  : Prog                                                        { constructor (Right (Map.empty,Secuencia[])) $1 }
       | LProg Prog                                                  { constructor $1 $2 }

Prog   : Decl '.'                                                    { detectarErrores($1, Secuencia []) }
       | Expr '.'                                                    { (Right (Map.empty, Expr $1)) }
       | Inst '.'                                                    { (Right (Map.empty, Expr (Instruccion $1)))}

Decl  : Lista_id es dominio Dominio                                  { insertarDominio $1 $4 }
      | Lista_id tiene dominio Dominio                               { insertarConjunto $1 $4 }

Dominio : ConjuntoDom                                                { Dominio $1 }
        | id                                                         { DominioID $1 }
        | universal                                                  { Dominio (elUniverso) }

ConjuntoDom : '{' '}'                                                { (SetC.emptySet)  }
            | '{' LAlfa '}'                                          { (SetC.fromList $2) }
            | '{' ListaConjDom '}'                                   { (SetC.fromList $2) }
            | '{' ListaTuplaDom '}'                                  { (SetC.fromList $2) }

LAlfa : str                                                          { [Elem (takeStr $1)] }
      | LAlfa ',' str                                                { $1 ++ [Elem (takeStr $3)] }

ConjDom : '{' '}'                                                    { Cto (SetC.emptySet) }
        | '{' LAlfa '}'                                              { Cto (SetC.fromList $2) }
        | '{' ListaConjDom '}'                                       { Cto (SetC.fromList $2) }

ListaConjDom : ConjDom                                               { [$1] }
             | ListaConjDom ',' ConjDom                              { $1 ++ [$3] }

TuplaDom : '[' str ',' str ']'                                       { Tupla (Elem (takeStr $2),Elem (takeStr $4)) }
         | '[' TuplaDom ',' TuplaDom ']'                             { Tupla ($2,$4) }
         | '[' ConjDom ',' ConjDom ']'                               { Tupla ($2,$4) }

ListaTuplaDom : TuplaDom                                             { [$1] } 
              | ListaTuplaDom ',' TuplaDom                           { $1 ++ [$3] }

Lista_id : id                                                        { [$1] }
         | Lista_id ',' id                                           { $1 ++ [$3] }

Conjunto : '{' '}'                                                   { Conjunto (SetC.emptySet) (Dominio (SetC.emptySet)) }
         | '{' Alfa_ran '}'                                          { Conjunto (SetC.fromList $2) (Dominio (SetC.emptySet)) }
         | '{' ListaConj '}'                                         { Conjunto (SetC.fromList $2) (Dominio (SetC.emptySet)) }
         | '{' ListaTupla '}'                                        { Conjunto (SetC.fromList $2) (Dominio (SetC.emptySet)) }

Alfa_ran : str                                                       { [Elem (takeStr $1)] }
         | str '..' str                                              { doRange $1 $3 }
         | Alfa_ran ',' Alfa_ran                                     { $1 ++ $3 }

Conj : '{' '}'                                                       { Cto (SetC.emptySet) }
     | '{' Alfa_ran '}'                                              { Cto (SetC.fromList $2) }
     | '{' ListaConj '}'                                             { Cto (SetC.fromList $2) }
     | '{' ListaTupla '}'                                            { Cto (SetC.fromList $2) }

ListaConj : Conj                                                     { [$1] }
          | ListaConj ',' Conj                                       { $1 ++ [$3] }

TuplaC : '['str ',' str ']'                                          { Tupla (Elem (takeStr $2),Elem (takeStr $4)) }
       | '[' TuplaC ',' TuplaC ']'                                   { Tupla ($2,$4) }
       | '[' Conj ',' Conj ']'                                       { Tupla ($2,$4) }

ListaTupla : TuplaC                                                  { [$1] }
           | ListaTupla ',' TuplaC                                   { $1 ++ [$3] }

Asig     : id ':=' Expr                                              { Asignacion $1 $3 }

Inst     : estado                                                    { Estado }
         | olvidar todo                                              { OlvidarTodo }
         | olvidar Lista_id                                          { Olvidar $2 }
         | fin                                                       { Fin }

Expr     : Conjunto                                                  { OpConj $1 }
         | id                                                        { OpId $1 }
         | Universo                                                  { OpUniverso $1 }
         | Extension                                                 { OpExtension $1}
         | Expr '+' Expr                                             { Union $1 $3 }
         | Expr '*' Expr                                             { Interseccion $1 $3 }
         | Expr '-' Expr                                             { Diferencia $1 $3}
         | '~' Expr                                                  { Complemento $2 }
         | Expr '%' Expr                                             { Cartesiano $1 $3 }
         | Expr '!'                                                  { Partes $1 }
         | Asig                                                      { $1 }
         | '(' Expr ')'                                              { $2 }


Universo : universo                                                  { UniversoT (crearUniverso) }
         | universo de id                                            { UniversoDe $3 }

Extension : '{' ExtElem '|' LGenerador ',' LFiltro '}'               { ConjuntoExt $2 $4 $6 }
          | '{' ExtElem '|' LGenerador '}'                           { ConjuntoExt $2 $4 [] }

ExtElem : id                                                         { Ident $1 }
        | '[' id ',' id ']'                                          { Tupla (Ident $2, Ident $4) }

LGenerador : Generador                                               { [$1] }
           | LGenerador ',' Generador                                { $1 ++ [$3] }

Generador : id '<-' id                                               { Gen (takeStr $1) $3 }

LFiltro : Filtro                                                     { [$1] }
        | LFiltro ',' Filtro                                         { $1 ++ [$3] }

Filtro : Elemento '==' Elemento                                      { FilIgual $1 $3 }
       | Elemento '<' Elemento                                       { FilMenor $1 $3 }
       | Elemento '>' Elemento                                       { FilMayor $1 $3 }
       | mayuscula Elemento                                          { FilMayuscula $2 }
       | letra Elemento                                              { FilLetra $2 }
       | digito Elemento                                             { FilDigito $2 }
       | simbolo Elemento                                            { FilSimbolo $2 }
       | not Filtro                                                  { FilNot $2 }
       | '(' Filtro ')'                                              { $2 }
       | miembro '(' Elemento ',' Conjunto ')'                       { Miembro $3 $5 }
       | vacio '(' Expr ')'                                          { Vacio $3 }
       | subconjunto '(' Conjunto ',' Conjunto ')'                   { SubConjunto $3 $5 }

Elemento : id                                                        { Ident $1 }
         | str                                                       { Elem (takeStr $1) }

{

{-|
                El tipo @TupParser@ se definió para evitar código sumamente largo
                e ilegible.
-}

type TupParser = (Map.Map Token Symbol, AST)

{-|
		La función @parser@ analiza los resultados generados por la función elNoParser
		y devuelve los errores generados en caso de haberlos y en caso contrario
                devuelve el Data.Map de todas las variables declaradas y el árbol
                sintáctivo generado por las expresiones del programa.
-}


parser :: [Token] -- ^ Lista de tokens que recibe del analizador lexicográfico
       -> (Map.Map String Symbol, AST) -- ^ Tupla que contiene el Data.Map y el árbol sintáctico.
parser toks = case elNoParser toks of
                Right (mapa,ast) -> (transformarMapa mapa,ast)
                Left (map,errs) -> error $ ("\n" ++ errs)

{-|
		La función @syntaxError@ es la función de error del analizador
                sintáctico generado por Happy y que permite manejar los errores
                de sintaxis de manera elegante y mostrar el token que generó el error
                y aquellos que le siguen.
-}

syntaxError :: [Token] -- ^ Lista de tokens que comienza con el token que generó el error seguido por los que no han sido analizados.
            -> a -- ^ Error que devuelve el token donde ocurrió el error, su línea, su columna y los 3 tokens que le siguen.
syntaxError (t:[]) = error $ "Error de sintaxis: La expresion no fue terminada con un punto"
syntaxError (t:ts) = error $ 
                       "Error de sintaxis en el Token " ++ (show t) ++ " en la linea " ++ show(fst(takePos t)) ++ " y en la columna " ++ show(snd(takePos t)) ++ "\n" ++
                       "Seguido de: " ++ (unlines $ map show $ take 3 ts)

{-|
                La función @constructor@ concatena las tuplas generadas por las
                declaraciones y expresiones del programa uniendo los mapas de simbolos
                y revisando los errores de contexto en el proceso, además guardando 
                los errores generados en todo el proceso de construcción.
-}
constructor :: Either ((Map.Map Token Symbol), String) TupParser -- ^ Tupla que puede ser TupParser en caso de no haber errores o en caso de haber errores de contexto,una tupla que contiene el Data.Map generado hasta el momento y un string con todos los errores concatenados.
            -> Either String TupParser -- ^ Es TupParser en caso de que la expresion o declaración analizada no tenga errores de contexto o un String en caso contrario.
            -> Either ((Map.Map Token Symbol), String) TupParser -- ^ Tupla que puede ser TupParser en caso de no haber errores o en caso de haber errores de contexto,una tupla que contiene el Data.Map generado hasta el momento y un string con todos los errores concatenados.

constructor tup1 tup2 = case tup1 of
                          Right (m, a) -> case tup2 of 
                                            Right (n, b) -> case unirMapas m n of
                                                              Right res -> Right (res, (construirAST a b))
                                                              Left err2 -> Left (m,err2)
                          Left (m, errs) -> case tup2 of
                                              Right (n, b) -> case unirMapas m n of
                                                                Right res -> Left (res, errs)
                                                                Left err2 -> Left (m, errs ++ "\n" ++ err2)
                                              Left err -> Left (m, errs ++ "\n" ++ err)
                                         

{-|
                La función @construirAST@ concatena dos árboles sintácticos
                AST generados por el analizador lexicográfico.
-}
                                               
construirAST :: AST -- ^ Árbol sintáctico a ser concatenado.
             -> AST -- ^ Árbol sintáctico a ser concatenado.
             -> AST -- ^ Árbol sintáctico que resulta de la concatenación de los otros AST.
construirAST (Expr a) (Expr b) = Secuencia [a, b]
construirAST (Expr a) (Secuencia b) = Secuencia ([a] ++ b)
construirAST (Secuencia a) (Expr b) = Secuencia (a ++ [b])
construirAST (Secuencia a) (Secuencia b) = Secuencia (a ++ b)

{-|
                La función @unirMapas@ realiza la unión de dos mapas
                de símbolos mientras revisa si hay errores de contexto.
-}

unirMapas :: Map.Map Token Symbol -- ^ Mapa de símbolos a ser unido.
          -> Map.Map Token Symbol -- ^ Mapa de símbolos a ser unido.
          -> Either String (Map.Map Token Symbol) -- ^ Devuelve el resultado de la unión de los dos mapas de símbolos en caso de no haber errores y un String con los errores en caso contrario.

unirMapas map1 map2 = unirMapas' map1 (Map.toList map2)

{-|
                La función @unirMapas'@ realiza la inserción
                de la lista de tuplas conformadas por variables y símbolos
                en un mapa de símbolos revisando si hay errores de contexto.
-}

unirMapas' :: Map.Map Token Symbol -- ^ Mapa de símbolos a la que se van a insertar los valores.
           -> [(Token, Symbol)] -- ^ Lista de tuplas formadas por variables y símbolos que será insertada en el mapa en caso de no haber errores de contexto.
           -> Either String (Map.Map Token Symbol) -- ^ Devuelve el resultado de la inserción de los valores en el mapa en caso de no haber errores, en caso contrario se devuelve un String con los errores.

unirMapas' map1 [] = Right map1
unirMapas' map1 (x:[]) = crearValor map1 x
unirMapas' map1 (x:xs) = case mapR of
                           Right map2 -> case unirMapas' map2 xs of
                                          Right m -> Right (Map.union map2 m)
                                          Left errs -> Left errs
                           Left err -> case unirMapas' map1 xs of
                                         Right m -> Left err
                                         Left errs -> Left (errs ++ err)
    where
      mapR = crearValor map1 x

{-|
                La función @crearValor@ decide, en función del valor introducido
                si se va a generar una entrada en el mapa para la variable
                como Conjunto o como Dominio y los genera e inserta en el mapa en caso de no haber
                errores de contexto y en caso contrario devuelve un String con
                los errores.
-}

crearValor :: Map.Map Token Symbol -- ^ Mapa de símbolos donde se va a insertar el valor.
           -> (Token, Symbol) -- ^ Tupla que puede tener un valor de Conjunto o de Dominio y que se va a intentar insertar en el mapa.
           -> Either String (Map.Map Token Symbol) -- ^ Devuelve un String en caso de haber errores y un mapa de símbolos con el valor nuevo introducido en caso contrario.

crearValor map x = case snd(x) of
                     Symbol(Just dom, _) -> crearDominio map (fst(x),Symbol(Just dom, Nothing))
                     Symbol(_, Just conj) -> crearConjunto map (fst(x),Symbol(Nothing, Just conj))
                     _ -> Left "Error 0x08042FF2"
                      
{-|
                La función @crearDominio@ intenta insertar en un mapa de símbolos
                una nueva entrada para una variable en su definición como Dominio,
                en caso de ya existir este valor se genera un error de contexto y en
                caso contrario se devuelve el mapa resultante.
-}

crearDominio :: Map.Map Token Symbol -- ^ Mapa donde se va a intentar insertar el valor.
             -> (Token, Symbol) -- ^ Entrada que se intentará insertar en el mapa de símbolos.
             -> Either String (Map.Map Token Symbol) -- ^ Devuelve el mapa resultante en caso de no haber errores de contexto y un String con el error en caso contrario.

crearDominio map (k, v) = case  Map.lookup (takeStr k) (transformarMapa map) of
      Just (Symbol (Just dom, Just con)) -> Left ("Existe una doble declaracion en la linea " ++ show(fst(pos)) ++ ", en la columna " ++ show(snd(pos)) ++", ya existe un dominio con el nombre de variable " ++ (takeStr k) ++ ".\n")
      Just (Symbol (Just dom, Nothing)) -> Left ("Existe una doble declaracion en la linea " ++ show(fst(pos)) ++ ", en la columna " ++ show(snd(pos)) ++", ya existe un dominio con el nombre de variable " ++ (takeStr k) ++ ".\n")
      Just (Symbol (Nothing, Just con)) -> Right (Map.union (Map.singleton k (Symbol (Just (takeDom v), Just con))) map)
      Nothing -> Right (Map.insert k (Symbol (Just (takeDom v), Nothing)) map)
    where 
      pos = takePos k

{-|
                La función @crearConjunto@ intenta insertar en un mapa de símbolos
                una nueva entrada para una variable en su definición como Conjunto,
                en caso de ya existir este valor se genera un error de contexto y en
                caso contrario se devuelve el mapa resultante.
-}

crearConjunto :: Map.Map Token Symbol -- ^ Mapa donde se va a intentar insertar el valor.
              -> (Token, Symbol) -- ^ Entrada que intentará insertar en el mapa de símbolos.
              -> Either String (Map.Map Token Symbol) -- ^ Devuelve el mapa resultante en caso de no haber errores de contexto y un String con el error en caso contrario.

crearConjunto map (k, v) = case  Map.lookup (takeStr k) (transformarMapa map) of
      Just (Symbol (Just dom, Just con)) -> Left ("Existe una doble declaracion en la linea " ++ show(fst(pos)) ++ ", en la columna " ++ show(snd(pos)) ++", ya existe un conjunto con el nombre de variable " ++ (takeStr k) ++ ".\n")
      Just (Symbol (Nothing, Just con)) -> Left ("Existe una doble declaracion en la linea " ++ show(fst(pos)) ++ ", en la columna " ++ show(snd(pos)) ++", ya existe un conjunto con el nombre de variable " ++ (takeStr k) ++ ".\n")
      Just (Symbol (Just dom, Nothing)) -> Right (Map.union (Map.singleton k (Symbol (Just dom , Just (takeConj v)))) map)
      Nothing -> Right (Map.insert k (Symbol (Nothing , Just (takeConj v))) map)

    where 
      pos = takePos k

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

{-|
  Recibe una lista de variables provenientes de una declaración y las inicializa
  como dominios, chequeando que no existan doble declaraciones en el contexto
  que se está manejando.
-}
insertarDominio :: [Token] -- ^ Lista de variables a inicializar 
                -> Dominio -- ^ Dominio al cual representarán dichas variables
                -> Either String (Map.Map Token Symbol) -- ^ El nuevo mapa de símbolos o un string con algún error.
insertarDominio [] dom = Right Map.empty
insertarDominio (x:xs) dom = case unirMapas' (Map.singleton x (Symbol (Just dom, Nothing))) (map (hacerTuplaDom dom) xs) of
                               Right map1 -> Right map1
                               Left errs -> Left  errs

{-|
  Crea una tupla que contiene el nombre de la variable pasada por parametro
  y un dominio que guarda el /SetC/ pasado (envuelto en Dominio).
-}
hacerTuplaDom :: Dominio  -- ^ Dominio a guardar.
              -> Token -- ^ Variable a la que se le olvidara el dominio 
              -> (Token, Symbol)
hacerTuplaDom dom x = (x, (Symbol (Just dom, Nothing)))

{-|
  Recibe una lista de variables provenientes de una declaración y las inicializa
  como conjuntos (vacíos para los efectos de esta entrega de proyecto), chequeando
  que no existan doble declaraciones en el contexto que se está manejando.
-}
insertarConjunto :: [Token] -- ^ Lista de variables a inicializar
                 -> Dominio -- ^ Dominio sobre el cual se quiere inicializar los conjuntos.
                 -> Either String (Map.Map Token Symbol) -- ^ El nuevo mapa de símbolos o un string con algún error.
insertarConjunto [] conj = Right Map.empty
insertarConjunto (x:xs) conj = case unirMapas' (Map.singleton x (Symbol (Nothing, Just (Conjunto (SetC.emptySet) conj)))) (map (hacerTuplaConj conj) xs) of
                                 Right map1 -> Right map1
                                 Left errs -> Left errs

{-|
  Crea una tupla que contiene el nombre de la variable pasada por parametro
  y un conjunto vacío.
-}
hacerTuplaConj :: Dominio
               -> Token -- ^ Variable a crear.
               -> (Token, Symbol) -- ^ Conjunto vacío
hacerTuplaConj dom x = (x, (Symbol (Nothing, Just (Conjunto (SetC.emptySet) (dom)))))

{-|
  Verifica que se haya recibido efectivamente un Mapa de símbolos
  y lo devuelve. Si no es así, devuelve únicamente los errores
  encontrados.
-}
detectarErrores :: ((Either String (Map.Map Token Symbol)), AST) 
                -> Either String TupParser
detectarErrores (map,ast) = case map of
                              Right map1 -> Right (map1,ast)
                              Left err -> Left err
                              

{-|
  Crea el universo de todos los caractéres alfanuméricos imprimibles
  en Haskell.
-}
elUniverso = SetC.fromList (map Elem (map (\c -> [c]) (filter isPrint ['\000'..'\177'])))

{-|
  Función que genera el conjunto de todos los caracteres imprimibles de Haskell.
-}
crearUniverso = Conjunto elUniverso (Dominio elUniverso)


doRange :: Token
        -> Token
        -> [Elemento]
doRange t1 t2 = map toElem [(head $ takeStr t1) .. (head $ takeStr t2)]
    where toElem c = Elem [c]


{-|
  Función que devuelve el string envuelto por un Token cuyo constructor
  sea TkStr o TkId.
-}
takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s

{-|
  Función que ejecuta las instrucciones especiales del lenguaje
-}
ejecutarInstruccion :: Inst -> (Map.Map k a, AST)
ejecutarInstruccion a = (Map.empty, Secuencia [])

{-|
  Devuelve la posición ocupada por un Token
-}
takePos :: Token -> (Int,Int)
takePos (TkEs a) = a
takePos (TkDe a) = a
takePos (TkTiene a) = a
takePos (TkDominio a) = a
takePos (TkUniverso a) = a
takePos (TkUniversal a) = a
takePos (TkALlave a) = a
takePos (TkCLlave a) = a
takePos (TkACorchete a) = a
takePos (TkCCorchete a) = a
takePos (TkAParentesis a) = a
takePos (TkCParentesis a) = a
takePos (TkComa a) = a
takePos (TkPunto a) = a
takePos (TkPuntoPunto a) = a
takePos (TkBarra a) = a
takePos (TkFlecha a) = a
takePos (TkAsignacion a) = a
takePos (TkUnion a) = a
takePos (TkInterseccion a) = a
takePos (TkDiferencia a) = a
takePos (TkComplemento a) = a
takePos (TkCartesiano a) = a
takePos (TkPartes a) = a
takePos (TkMiembro a) = a
takePos (TkVacio a) = a
takePos (TkSubconjunto a) = a
takePos (TkEstado a) = a
takePos (TkOlvidar a) = a
takePos (TkTodo a) = a
takePos (TkFin a) = a
takePos (TkIgual a) = a
takePos (TkMenor a) = a
takePos (TkMayor a) = a
takePos (TkMayuscula a) = a
takePos (TkLetra a) = a
takePos (TkDigito a) = a
takePos (TkSimbolo a) = a
takePos (TkNegar a) = a
takePos (TkId pos a) = pos
takePos (TkStr pos a) = pos


{-|
  Transforma un mapa que contiene claves de tipo Token
  a otro mapa que contiene claves del tipo String, siendo
  estos strings los que los Tokens guardan.
-}
transformarMapa :: Map.Map Token Symbol
                -> Map.Map String Symbol
transformarMapa mapa = Map.fromList (obtenerStrings (Map.toList mapa))

obtenerStrings :: [(Token,Symbol)]
               -> [(String,Symbol)]
obtenerStrings [] = []
obtenerStrings ((t,sym):[]) = [((takeStr t),sym)]
obtenerStrings ((t,sym):xs) = [((takeStr t),sym)] ++ obtenerStrings xs

}