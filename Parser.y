{
{-|
  /Analizador Sintáctico y Tabla de Símbolos para la Calculadora/


	Traductores e Interpretadores CI3725

	integrantes


	Este módulo, desarrollado en Happy, implanta un Analizador
	Sintáctico para la calculadora vista en clase, y un conjunto
	de rutinas Haskell para manipular una Tabla de Símbolos simple.
	Sirve como ejemplo de uso tanto para Happy como para Haddock e
	ilustra los conceptos de tipos recursivos en Haskell necesarios
	para la Parte Dos del Proyecto.

-}
module Parser (
	-- * Funciones exportadas.
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
      | Lista_id tiene dominio id                                    { insertarConjunto $1 (Dominio (SetC.fromList [Ident (Var (takeStr $4))])) }

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

Lista_id : id                                                        { [Var (takeStr $1)] }
         | Lista_id ',' id                                           { $1 ++ [Var (takeStr $3)] }

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

Asig     : id ':=' Expr                                            { Asignacion (Var (takeStr $1)) $3 }

Inst     : estado                                                    { Estado }
         | olvidar todo                                              { OlvidarTodo }
         | olvidar Lista_id                                          { Olvidar $2 }
         | fin                                                       { Fin }

Expr     : Conjunto                                                  { OpConj $1 }
         | id                                                        { OpId (Var (takeStr $1)) }
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
         | universo de id                                            { UniversoDe (Var (takeStr $3)) }

Extension : '{' ConjuntoId '|' LGenerador ',' LFiltro '}'            { ConjuntoExt $2 $4 $6 }
          | '{' ConjuntoId '|' LGenerador '}'                        { ConjuntoExt $2 $4 [] }

ConjuntoId : id                                                      { (SetC.fromList [(Ident (Var (takeStr $1)))]) }
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

Generador : id '<-' id                                               { Gen (Var (takeStr $1)) (Var (takeStr $3)) }

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

Elemento : id                                                        { Ident (Var (takeStr $1)) }
         | str                                                       { Elem (takeStr $1) }

{

{-
		La función @insertVar@ es de uso interno en la acción
		semántica asociada a las producciones que reconocen una
		lista de declaraciones de variables, para tomar el identificador
		'i' y la expresión 'e' que tendrá asociada e insertarlos en
		la Tabla de Símbolos.

		Si la Tabla de Símbolos ya contiene el identificador, eso
		constituye un Error Estático que se reporta inmediatamente,
		abortando la ejecución del Analizador Sintáctico.

		Si la Tabla de Símbolos no contiene el identificador, se utiliza
		la primitiva de inserción sobre Tablas de Símbolo, teniendo cuidado
		de "envolver" la expresión con el constructor Just del tipo
		Maybe Expr.

-}
type TupParser = (Map.Map Var Symbol, AST)

parser :: [Token] -> TupParser
parser toks = case elNoParser toks of
                Right tup -> tup
                Left (map,errs) -> error $ errs


syntaxError :: [Token] -> a
syntaxError (t:ts) = error $ 
                       "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
                       "Seguido de: " ++ (unlines $ map show $ take 3 ts)

{-|
  constructor
  
  Funci�n que concatena listas encapsuladas en tuplas.
-}
constructor :: Either ((Map.Map Var Symbol), String) TupParser -> Either String TupParser -> Either ((Map.Map Var Symbol), String) TupParser
constructor tup1 tup2 = case tup1 of
                          Right (m, a) -> case tup2 of 
                                            Right (n, b) -> case Map.null (chequearAsignacion b) of
                                                       True -> case unirMapas m n of
                                                                 Right res -> Right (res, (construirAST a b))
                                                                 Left err2 -> Left (m,err2)
                                                       False -> case (unirMapas (actualizarMapa m (Map.toList (chequearAsignacion b))) n) of
                                                                  Right res -> Right (res, construirAST a b)
                                                                  Left err2 -> Left (m,err2)
                                            Left errs -> Left (m,errs)
                          Left (m, errs) -> case tup2 of
                                            Right (n, b) -> case Map.null (chequearAsignacion b) of
                                                       True -> case unirMapas m n of
                                                                 Right res -> Right (res, b)
                                                                 Left err2 -> Left (m, errs ++ "\n" ++ err2)
                                                       False -> case (unirMapas (actualizarMapa m (Map.toList (chequearAsignacion b))) n) of
                                                                  Right res -> Right (res, b)
                                                                  Left err2 -> Left (m, errs ++ "\n" ++ err2)
                                            Left err -> Left (m, errs ++ "\n" ++ err)
                                         
                                               
construirAST :: AST -> AST -> AST
construirAST (Expr a) (Expr b) = Secuencia [a, b]
construirAST (Expr a) (Secuencia b) = Secuencia ([a] ++ b)
construirAST (Secuencia a) (Expr b) = Secuencia (a ++ [b])
construirAST (Secuencia a) (Secuencia b) = Secuencia (a ++ b)

unirMapas :: Map.Map Var Symbol -> Map.Map Var Symbol -> Either String (Map.Map Var Symbol)
unirMapas map1 map2 = unirMapas' map1 (Map.toList map2)

unirMapas' :: Map.Map Var Symbol -> [(Var, Symbol)] -> Either String (Map.Map Var Symbol)
unirMapas' map1 [] = Right map1
unirMapas' map1 (x:[]) = crearValor map1 x
unirMapas' map1 (x:xs) = case mapR of
                           Right map2 -> case unirMapas' map2 xs of
                                          Right m -> Right (Map.union map2 m)
                                          Left errs -> Left errs
                           Left err -> case unirMapas' map1 xs of
                                         Right m -> Left err
                                         Left errs -> Left (errs ++ "\n" ++ err)
    where
      mapR = crearValor map1 x

crearValor :: Map.Map Var Symbol -> (Var, Symbol) -> Either String (Map.Map Var Symbol)
crearValor map x = case snd(x) of
                     Symbol(Just dom, _) -> crearDominio map (fst(x),Symbol(Just dom, Nothing))
                     Symbol(_, Just conj) -> crearConjunto map (fst(x),Symbol(Nothing, Just conj))
                     _ -> Left "Error 0x08042FF2"
                      

crearDominio :: Map.Map Var Symbol -> (Var, Symbol) -> Either String (Map.Map Var Symbol)
crearDominio map (k, v) =
    case  Map.lookup k map of
      Just (Symbol (Just dom, Just con)) -> Left ("La variable " ++ ((\(Var s) -> s) k) ++ " ya esta definida.")
      Just (Symbol (Just dom, Nothing)) -> Left ("La variable " ++ ((\(Var s) -> s) k) ++ " ya esta definida.X")
      Just (Symbol (Nothing, Just con)) -> Right (Map.union (Map.singleton k (Symbol (Just (takeDom v), Just con))) map)
      Nothing -> Right (Map.insert k (Symbol (Just (takeDom v), Nothing)) map)

crearConjunto :: Map.Map Var Symbol -> (Var, Symbol) -> Either String (Map.Map Var Symbol)
crearConjunto map (k, v) =
    case  Map.lookup k map of
      Just (Symbol (Just dom, Just con)) -> Left ("La variable " ++ ((\(Var s) -> s) k) ++ " ya esta definida.")
      Just (Symbol (Nothing, Just con)) -> Left ("La variable " ++ ((\(Var s) -> s) k) ++ " ya esta definida.X")
      Just (Symbol (Just dom, Nothing)) -> Right (Map.union (Map.singleton k (Symbol (Just dom , Just (takeConj v)))) map)
      Nothing -> Right (Map.insert k (Symbol (Nothing , Just (takeConj v))) map)

actualizarMapa :: Map.Map Var Symbol -> [(Var, Symbol)] -> Map.Map Var Symbol
actualizarMapa map1 ((m2key, m2value):[]) = actualizarMapa' m2key map1
actualizarMapa map1 ((m2key, m2value):m2s) = Map.union (actualizarMapa' m2key map1) (actualizarMapa map1 m2s)

actualizarMapa' :: Var -> Map.Map Var Symbol -> Map.Map Var Symbol
actualizarMapa' key map = case Map.lookup key map of
                           Just (Symbol (_, Just con)) -> map
                           Just (Symbol (_, Nothing)) -> "La variable " ++ ((\(Var s) -> s) key) ++ " no esta definida."
                           Nothing -> "La variable " ++ ((\(Var s) -> s) key) ++ " no esta definida."

takeDom :: Symbol -> Dominio
takeDom (Symbol (Just a, _)) = a
takeDom (Symbol (Nothing, _)) = error $ "hola, no deberia pasar"

takeConj :: Symbol -> Conjunto
takeConj (Symbol (_, Just a)) = a
takeConj (Symbol (_, Nothing)) = error $ "hola, no deberia pasar"

-- --------------------------------
-- INSERTAR COSAS
insertarDominio :: [Var] -> Dominio -> Either String (Map.Map Var Symbol)
insertarDominio [] dom = Right Map.empty
insertarDominio (x:xs) dom = case unirMapas' (Map.singleton x (Symbol (Just dom, Nothing))) (map (hacerTuplaDom dom) xs) of
                               Right map1 -> Right map1
                               Left errs -> Left  errs

hacerTuplaDom :: Dominio -> Var -> (Var, Symbol)
hacerTuplaDom dom x = (x, (Symbol (Just dom, Nothing)))

insertarConjunto :: [Var] -> Dominio -> Either String (Map.Map Var Symbol)
insertarConjunto [] conj = Right Map.empty
insertarConjunto (x:xs) conj = case unirMapas' (Map.singleton x (Symbol (Nothing, Just (Conjunto (SetC.emptySet))))) (map (hacerTuplaConj) xs) of
                                 Right map1 -> Right map1
                                 Left errs -> Left errs

hacerTuplaConj :: Var -> (Var, Symbol)
hacerTuplaConj x = (x, (Symbol (Nothing, Just (Conjunto (SetC.emptySet)))))

existeConjunto var map = case Map.lookup var map of
                           Just (a, Just cto) -> Just (a, Just cto)
                           Just (a, Nothing) -> Nothing
                           Nothing -> Nothing


chequearAsignacion :: AST -> Map.Map Var Symbol
chequearAsignacion (Secuencia []) = Map.empty
chequearAsignacion (Expr exp) = chequearAsignacion' exp Map.empty
chequearAsignacion (Secuencia (x:[])) = chequearAsignacion' x Map.empty
chequearAsignacion (Secuencia (x:xs)) = Map.union (chequearAsignacion' x Map.empty) (chequearAsignacion (Secuencia xs))

chequearAsignacion' :: Expresion -> Map.Map Var Symbol -> Map.Map Var Symbol
chequearAsignacion' exp map = case exp of
                               Union x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Interseccion x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Diferencia x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Cartesiano x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Partes x -> Map.union map (chequearAsignacion' x map)
                               Complemento x -> Map.union map (chequearAsignacion' x map)
                               OpExtension (ConjuntoExt set gens fils) -> Map.union map (chequearGenerador gens)
                               Asignacion var x -> Map.union (Map.insert var (Symbol (Nothing, Just (Conjunto (SetC.emptySet)))) map) (chequearAsignacion' x map)
                               _ -> Map.empty


chequearGenerador :: [Generador] -> Map.Map Var Symbol
chequearGenerador [] = Map.empty
chequearGenerador ((Gen x y):xs) = Map.union (Map.singleton y (Symbol (Nothing, Just (Conjunto (SetC.emptySet))))) (chequearGenerador xs)

detectarErrores :: ((Either String (Map.Map Var Symbol)), AST) -> Either String TupParser
detectarErrores (map,ast) = case map of
                              Right map1 -> Right (map1,ast)
                              Left err -> Left err
                              

-- --------------
-- FUNCIONES RANDOM
doList [(Lista a)] [(Lista b)] = [Lista a, Lista b]
doCto [(Cto a)] [(Cto b)] = [Cto a, Cto b]

crearUniverso = SetC.fromList (map Elem (map (\c -> [c]) (filter isPrint ['\000'..'\177'])))

takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s

-- -------------------
-- Instrucciones
ejecutarInstruccion :: Inst -> (Map.Map k a, AST)
ejecutarInstruccion a = (Map.empty, Secuencia [])
}
