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

%name parser
%error { syntaxError }
%tokentype { Token }
%token
        es                       { TkEs a            }
        de                       { TkDe a            }
        tiene                    { TkTiene a         }
        dominio                  { TkDominio a       }
        conjunto                 { TkConjunto a      }
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

LProg  : Prog                                                        { $1 }
       | LProg Prog                                                  { constructor $1 $2 }

Prog   : Decl '.'                                                    { ($1, Secuencia []) }
       | Expr '.'                                                    { (Map.empty, Expr $1) }

Decl  : Lista_id es dominio Dominio                                  { insertarDominio $1 $4 }
      | Lista_id tiene dominio Dominio                               { insertarConjunto $1 $4 }
      | Lista_id tiene dominio id                                    { insertarConjunto $1 (Dominio (SetC.fromList [Ident (Var (takeStr $4))])) }

Expr  : Instr                                                        { Instruccion $1 }
      | OpConj                                                       { Operacion $1 }

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
         | Alfa_ran ',' str                                          { $1 ++ [Elem (takeStr $3)] }

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

Asig     : id ':=' OpConj                                            { Asignacion (Var (takeStr $1)) $3 }

Instr    : estado                                                    { Estado }
         | olvidar todo                                              { OlvidarTodo }
         | olvidar Lista_id                                          { Olvidar $2 }
         | fin                                                       { Fin }

OpConj   : Conjunto                                                  { OpConj $1 }
         | id                                                        { OpId (Var (takeStr $1)) }
         | Universo                                                  { OpUniverso $1 }
         | Extension                                                 { OpExtension $1}
         | OpConj '+' OpConj                                         { Union $1 $3 }
         | OpConj '*' OpConj                                         { Interseccion $1 $3 }
         | OpConj '-' OpConj                                         { Diferencia $1 $3}
         | '~' OpConj                                                { Complemento $2 }
         | OpConj '%' OpConj                                         { Cartesiano $1 $3 }
         | OpConj '!'                                                { Partes $1 }
         | '(' OpConj ')'                                            { $2 }
         | Asig                                                      { $1 }

Universo : universo                                                  { UniversoT (Conjunto (crearUniverso)) }
         | universo de id                                            { UniversoDe (Var (takeStr $3)) }

Extension : '{' ConjuntoId '|' LGenerador ',' LFiltro '}'            { ConjuntoExt $2 $4 $6 }
          | '{' ConjuntoId '|' LGenerador '}'                        { ConjuntoExt $2 $4 [] }

ConjuntoId : id                                                      { (SetC.fromList [(Ident (Var (takeStr $1)))]) }
           | ListaArrAlfaId                                          { (SetC.fromList $1) }
           | ListaConjAlfaId                                         { (SetC.fromList $1) }

ListaArrAlfaId : '[' ']'                                             { [Lista []] }
               | '['ListaAlfaId ']'                                  { [Lista $2] }
               | ListaArrAlfaId ',' ListaArrAlfaId                   { doList $1 $3 }
               | '['ListaArrAlfaId ']'                               { [Lista $2] }
               | '['ListaConjAlfaId ']'                              { [Lista $2] }

ListaConjAlfaId : '{' '}'                                            { [Cto (SetC.emptySet)] }
                | '{' ListaAlfaId '}'                                { [Cto (SetC.fromList $2)] }
                | ListaConjAlfaId ',' ListaConjAlfaId                { doCto $1 $3 }
                | '{' ListaConjAlfaId '}'                            { [Cto (SetC.fromList $2)] }
                | '{' ListaArrAlfaId '}'                             { [Cto (SetC.fromList $2)] }

ListaAlfaId : Elemento                                               { [$1] }
            | ListaAlfaId ',' Elemento                               { $1 ++ [$3] }

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
       | vacio '(' OpConj ')'                                        { Vacio $3 }
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
syntaxError :: [Token] -> a
syntaxError (t:ts) = error $ 
                       "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
                       "Seguido de: " ++ (unlines $ map show $ take 3 ts)

{-|
  constructor
  
  Funci�n que concatena listas encapsuladas en tuplas.
-}
constructor (m, a) (n, b) = ((Map.union m n), (construirAST a b))

construirAST :: AST -> AST -> AST
construirAST (Expr a) (Expr b) = Secuencia [a, b]
construirAST (Expr a) (Secuencia b) = Secuencia ([a] ++ b)
construirAST (Secuencia a) (Expr b) = Secuencia (a ++ [b])
construirAST (Secuencia a) (Secuencia b) = Secuencia (a ++ b)

-- --------------
doList [(Lista a)] [(Lista b)] = [Lista a, Lista b]
doCto [(Cto a)] [(Cto b)] = [Cto a, Cto b]

crearUniverso = SetC.fromList (map Elem (map (\c -> [c]) (filter isPrint ['\000'..'\177'])))

takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s

-- --------------------------------

-- Faltan los chequeos
insertarDominio (x:[]) dom = Map.singleton x dom
insertarDominio (x:xs) dom = Map.union (Map.singleton x dom) (insertarDominio xs dom)

insertarConjunto (x:[]) dom = Map.singleton x (Conjunto (SetC.emptySet))
insertarConjunto (x:xs) dom = Map.union (Map.singleton x (Conjunto (SetC.emptySet))) (insertarDominio xs dom)

}
