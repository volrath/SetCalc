{
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

%left '+' '-' '*' '%'
%nonassoc '~'
%nonassoc '!'

%left ','
%right ':='

%%

LProg  : Prog                                                        { constructor (Right (Map.empty,Secuencia[])) $1 }
       | LProg Prog                                                  { constructor $1 $2 }

Prog   : Decl '.'                                                    { detectarErrores($1, Secuencia []) }
       | Expr '.'                                                    { (Right (Map.empty, Expr $1)) }
       | Inst '.'                                                    { (Right (ejecutarInstruccion $1))}

Decl  : Lista_id es dominio Dominio                                  { insertarDominio $1 $4 }
      | Lista_id tiene dominio Dominio                               { insertarConjunto $1 $4 }
      | Lista_id tiene dominio id                                    { insertarConjunto $1 (Dominio (SetC.fromList [Ident $4])) }

Dominio : ConjuntoDom                                                { Dominio $1 }
        | universal                                                  { Dominio (crearUniverso) }

Conjunto : ConjuntoCto                                               { Conjunto $1 }

ConjuntoDom : '{' '}'                                                { (SetC.emptySet) }
            | '{' LAlfa '}'                                          { (SetC.fromList $2) }
            | '{' ListaConjDom '}'                                   { (SetC.fromList $2) }
            | '{' ListaArregloDom '}'                                { (SetC.fromList $2) }

LAlfa : str                                                          { [Elem (takeStr $1)] }
      | LAlfa ',' str                                                { $1 ++ [Elem (takeStr $3)] }

ListaConjDom : '{' '}'                                               { [Cto (SetC.emptySet)] }
             | '{' LAlfa '}'                                         { [Cto (SetC.fromList $2)] }
             | ListaConjDom ',' ListaConjDom                         { doCto $1 $3 }
             | '{' ListaConjDom '}'                                  { [Cto (SetC.fromList $2)] }

ListaArregloDom : '[' ']'                                            { [Lista []] }
                | '[' LAlfa ']'                                      { [Lista $2] }
                | ListaArregloDom ',' ListaArregloDom                { doList $1 $3 }
                | '[' ListaArregloDom ']'                            { [Lista $2] }

Lista_id : id                                                        { [$1] }
         | Lista_id ',' id                                           { $1 ++ [$3] }

ConjuntoCto : '{' '}'                                                { (SetC.emptySet) }
            | '{' Alfa_ran '}'                                       { (SetC.fromList $2) }
            | '{' ListaConj '}'                                      { (SetC.fromList $2) }
            | '{' ListaArreglo '}'                                   { (SetC.fromList $2) }

Alfa_ran : str                                                       { [Elem (takeStr $1)] }
         | str '..' str                                              { [Rango (head $ takeStr $1) (head $ takeStr $3)] }
         | Alfa_ran ',' Alfa_ran                                     { $1 ++ $3 }

ListaConj : '{' '}'                                                  { [Cto (SetC.emptySet)] }
          | '{' Alfa_ran '}'                                         { [Cto (SetC.fromList $2)] }
          | ListaConj ',' ListaConj                                  { doCto $1 $3 }
          | '{' ListaConj '}'                                        { [Cto (SetC.fromList $2)] }
          | '{' ListaArreglo '}'                                     { [Cto (SetC.fromList $2)] }

ListaArreglo : '[' ']'                                               { [Lista []] }
             | '[' Alfa_ran ']'                                      { [Lista $2] }
             | ListaArreglo ',' ListaArreglo                         { doList $1 $3 }
             | '[' ListaArreglo ']'                                  { [Lista $2] }
             | '[' ListaConj ']'                                     { [Lista $2] }

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
         | '(' Expr ')'                                              { $2 }
         | Asig                                                      { $1 }

Universo : universo                                                  { UniversoT (Conjunto (crearUniverso)) }
         | universo de id                                            { UniversoDe $3 }

Extension : '{' ConjuntoId '|' LGenerador ',' LFiltro '}'            { ConjuntoExt $2 $4 $6 }
          | '{' ConjuntoId '|' LGenerador '}'                        { ConjuntoExt $2 $4 [] }

ConjuntoId : id                                                      { (SetC.fromList [Ident $1]) }
           | ListaArrAlfaId                                          { (SetC.fromList $1) }
           | ListaConjAlfaId                                         { (SetC.fromList $1) }

ListaArrAlfaId : '['Lista_id ']'                                     { [Lista (map Ident $2)] }
               | ListaArrAlfaId ',' ListaArrAlfaId                   { doList $1 $3 }
               | '['ListaArrAlfaId ']'                               { [Lista $2] }
               | '['ListaConjAlfaId ']'                              { [Lista $2] }

ListaConjAlfaId : '{' Lista_id '}'                                   { [Cto (SetC.fromList (map Ident $2))] }
                | ListaConjAlfaId ',' ListaConjAlfaId                { doCto $1 $3 }
                | '{' ListaConjAlfaId '}'                            { [Cto (SetC.fromList $2)] }
                | '{' ListaArrAlfaId '}'                             { [Cto (SetC.fromList $2)] }

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
                                            Right (n, b) -> case Map.null (chequearAsignacion b) of
                                                       True -> case unirMapas m n of
                                                                 Right res -> Right (res, (construirAST a b))
                                                                 Left err2 -> Left (m,err2)
                                                       False -> case actualizarMapa m (Map.toList (chequearAsignacion b)) of
                                                                 Right res -> case unirMapas res n of
                                                                               Right res2 -> Right (res2, construirAST a b)
                                                                               Left err2 -> Left (m,err2)
                                                                 Left (m2,err) -> case unirMapas m2 n of
                                                                                   Right res2 -> Left (res2, err)
                                                                                   Left err2 -> Left (m, err ++ "\n" ++ err2)
                                            Left errs -> Left (m,errs)
                          Left (m, errs) -> case tup2 of
                                            Right (n, b) -> case Map.null (chequearAsignacion b) of
                                                       True -> case unirMapas m n of
                                                                 Right res -> Left (res, errs)
                                                                 Left err2 -> Left (m, errs ++ "\n" ++ err2)
                                                       False -> case actualizarMapa m (Map.toList (chequearAsignacion b)) of
                                                                 Right res -> case unirMapas res n of
                                                                               Right res2 -> Left (res2, errs)
                                                                               Left err2 -> Left (m, errs ++ "\n" ++ err2)
                                                                 Left (m2,err) -> case unirMapas m2 n of
                                                                                   Right res2 -> Left (res2,errs ++ "\n" ++ err)
                                                                                   Left err2 -> Left (m, errs ++ "\n" ++ err2 ++ "\n" ++ err)
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
      pos = takePosIdStr k

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
      pos = takePosIdStr k

{-|
  La función @actualizarMapa@ recibe un Mapa de símbolos
  y una lista de tuplas de entradas que representan 
  las variables usadas en las asignaciones y en los generadores
  que se encuentran en las expresiones del programa
  y mediante el uso de la función actualizarMapa' se
  genera un error en caso de haberse usado una variable no
  definida y se devuelve el mapa resultante en caso contrario.
-}
actualizarMapa :: Map.Map Token Symbol -- ^ Mapa generado por las declaraciones del programa hasta el momento.
               -> [(Token, Symbol)]  -- ^ Lista de tuplas que fueron usadas en asignaciones y en generadores.
               -> Either ((Map.Map Token Symbol), String) (Map.Map Token Symbol) -- ^ Devuelve una tupla con el mapa generado hasta el momento con las asignaciones que han sido aceptadas acompañada de un String con los errores en caso de haber errores de contexto, en caso contrario devuelve el mapa resultante de realizar las asignaciones.
actualizarMapa map1 ((m2key, m2value):[]) = case actualizarMapa' m2key map1 of
                                             Right res -> Right res
                                             Left err -> Left (map1,err)
actualizarMapa map1 ((m2key, m2value):m2s) = case actualizarMapa' m2key map1 of
                                              Right res -> case actualizarMapa map1 m2s of 
                                                            Right res1 -> Right (Map.union res res1)
                                                            Left (m,err) -> Left (Map.union res m, err)
                                              Left err -> case actualizarMapa map1 m2s of
                                                           Right res1 -> Left (res1,err)
                                                           Left (m,err1) -> Left (m,err++err1)

{-|
  Recibe una variable y un mapa de símbolos y revisa si dicha
  variable se encuentra definida en el mapa como un conjunto.
  Si la encuentra devuelve el mapa de símbolos (actualizado
  para la siguiente entrega), pero si no lo encuentra devuelve
  un error correspondiente.
-}
actualizarMapa' :: Token -- ^ Variable a chequear
                -> Map.Map Token Symbol -- ^ Mapa en donde se buscará la variable
                -> Either String (Map.Map Token Symbol)-- ^ Si se encontró la variable se devuelve el mapa actualizado. Error en caso contrario.
actualizarMapa' key map = case Map.lookup (takeStr key) (transformarMapa map) of
                           Just (Symbol (_, Just con)) -> Right map
                           Just (Symbol (_, Nothing)) -> Left ("La variable " ++ takeStr key ++ " no esta definida como conjunto y es usada en la linea "++ show(fst(pos)) ++ " y en la columna "++ show(snd(pos)) ++ ". \n")
                           Nothing -> Left ("La variable " ++ takeStr key ++ " no esta definida como conjunto y es usada en la linea "++ show(fst(pos)) ++ " y en la columna "++ show(snd(pos)) ++ ". \n")
    where
      pos = takePosIdStr key
      
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
insertarConjunto (x:xs) conj = case unirMapas' (Map.singleton x (Symbol (Nothing, Just (Conjunto (SetC.emptySet))))) (map (hacerTuplaConj) xs) of
                                 Right map1 -> Right map1
                                 Left errs -> Left errs

{-|
  Crea una tupla que contiene el nombre de la variable pasada por parametro
  y un conjunto vacío.
-}
hacerTuplaConj :: Token -- ^ Variable a crear.
               -> (Token, Symbol) -- ^ Conjunto vacío
hacerTuplaConj x = (x, (Symbol (Nothing, Just (Conjunto (SetC.emptySet)))))

{-|
  Dado un AST, revisa toda la lista de expresiones que éste contenga y
  las analiza con chequearAsignacion' para buscar todas las asignaciones
  dentro de un AST completo.
-}
chequearAsignacion :: AST -- ^ AST a analizar
                   -> Map.Map Token Symbol-- ^ Mapa de símbolos resultante
chequearAsignacion (Secuencia []) = Map.empty
chequearAsignacion (Expr exp) = chequearAsignacion' exp Map.empty
chequearAsignacion (Secuencia (x:[])) = chequearAsignacion' x Map.empty
chequearAsignacion (Secuencia (x:xs)) = Map.union (chequearAsignacion' x Map.empty) (chequearAsignacion (Secuencia xs))

{-|
  Recorre un árbol de expresiones en búsqueda de asignaciones, cuando
  las encuentra, va almacenando en un mapa de símbolos auxiliar las
  variables involucradas en dichas funciones. Este mapa de símbolos
  resultante sera contrastado al final con el mapa de símbolos resultante
  del análisis sintáctico del resto del programa.
-}
chequearAsignacion' :: Expresion -- ^ Árbol de expresiones.
                    -> Map.Map Token Symbol -- ^ Almacenamiento del mapa de símbolos a generar
                    -> Map.Map Token Symbol -- ^ Mapa de símbolos resultante
chequearAsignacion' exp map = case exp of
                               Union x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Interseccion x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Diferencia x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Cartesiano x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Partes x -> Map.union map (chequearAsignacion' x map)
                               Complemento x -> Map.union map (chequearAsignacion' x map)
                               OpExtension (ConjuntoExt set gens fils) -> Map.union map (chequearGenerador gens)
                               Asignacion var x -> Map.union (Map.insert var (Symbol (Nothing, Just (Conjunto (SetC.emptySet)))) map) (chequearAsignacion' x map)
                               OpId var -> Map.insert var (Symbol (Nothing, Just (Conjunto (SetC.emptySet)))) map
                               OpUniverso(UniversoDe var) -> Map.insert var (Symbol (Nothing, Just (Conjunto (SetC.emptySet)))) map
                               _ -> Map.empty

{-|
  Crea un mapa de símbolos con las variables usadas en los
  lados derechos de cada generador de un conjunto por extensión,
  este mapa luego será constrastado contra el mapa de símbolos
  original que se ha parseado.
-}
chequearGenerador :: [Generador] -- ^ Lista de generadores  
                  -> Map.Map Token Symbol -- ^ Mapa generado con las variables de los lados derechos de los generadores.
chequearGenerador [] = Map.empty
chequearGenerador ((Gen x y):xs) = Map.union (Map.singleton y (Symbol (Nothing, Just (Conjunto (SetC.emptySet))))) (chequearGenerador xs)

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
  Crea la concatenación de dos /Listas/
-}
doList [(Lista a)] [(Lista b)] = [Lista a, Lista b]

{-|
  Crea la concatenación de dos /Conjuntos/
-}
doCto [(Cto a)] [(Cto b)] = [Cto a, Cto b]

{-|
  Crea el universo de todos los caractéres alfanuméricos imprimibles
  en Haskell.
-}
crearUniverso = SetC.fromList (map Elem (map (\c -> [c]) (filter isPrint ['\000'..'\177'])))

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
  Devuelve la posición ocupada por Tokens cuyo
  constructor sea TkStr o TkId
-}
takePosIdStr :: Token -> (Int,Int)
takePosIdStr (TkStr pos s) = pos
takePosIdStr (TkId pos s) = pos

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
