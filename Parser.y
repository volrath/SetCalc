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
import Abstract
import qualified Data.Map as Map
}

%name parser
%error { syntaxError }
%tokentype { Token }
%token
        es                       { TkEs a            }
        de                       { TkDe  a           }
        tiene                    { TkTiene a         }
        dominio                  { TkDominio a       }
        conjunto                 { TkConjunto a      }
        universo                 { TkUniverso a      }
        universal                { TkUniversal a     }
        str                      { TkStr pos s         }
        id                       { TkId pos s          }
        '{'                      { TkALlave a         }
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
        todo                     { TkTodo  a         }
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
%right '~'
%left '!'

%left ','

%%

LProg  : Prog                                      { $1 }
       | LProg '.' Prog                            { concatTup $1 $3 }

Prog   : Decl                                      { ([$1], []) }
       | Expr                                      { ([], [$1]) }

Decl  : Lista_id es dominio Dominio                { ($1, $4) }
      | Lista_id tiene dominio Dominio             { ($1, $4) }
      | Lista_id tiene dominio id                  { ($1, $4) }

Dominio : ConjuntoDom                              { Dominio $1 }
        | universal                                { Dominio [] }

ConjuntoDom : '{' '}'                              { [] }
            | '{' LAlfa '}'                        { $2 }
            | '{' ListaConjDom '}'                 { $2 }
            | '{' ListaArregloDom '}'              { $2 }

LAlfa : str                                        { [Elem (takeStr $1)] }
      | LAlfa ',' str                              { $1 ++ [Elem (takeStr $3)] }

ListaConjDom : '{' '}'                             { [] }
             | '{' LAlfa '}'                       { $2 }
             | ListaConjDom ',' ListaConjDom       { $1 ++ $3 }
             | '{' ListaConjDom '}'                { $2 }

ListaArregloDom : '[' ']'                               { [] }
                | '[' LAlfa ']'                         { $2 }
                | ListaArregloDom ',' ListaArregloDom   { $1 ++ $3 }
                | '[' ListaArregloDom ']'               { $2 }

Lista_id : id                                      { [Var (takeStr $1)] }
         | Lista_id ',' id                         { $1 ++ [Var (takeStr $3)] }

Conjunto : '{' '}'                                 { Conjunto [] }
         | '{' Alfa_ran '}'                        { Conjunto $2 }
         | '{' ListaConj '}'                       { Conjunto $2 }
         | '{' ListaArreglo '}'                    { Conjunto $2 }

Alfa_ran : str                                     { [Elem (takeStr $1)] }
         | str '..' str                            { [Rango (head $ takeStr $1) (head $ takeStr $3)] }
         | Alfa_ran ',' str                        { $1 ++ [Elem (takeStr $3)] }

ListaConj : '{' '}'                                { [] }
          | '{' Alfa_ran '}'                       { $2 }
          | ListaConj ',' ListaConj                { $1 ++ $3 }
          | '{' ListaConj '}'                      { $2 }

ListaArreglo : '[' ']'                             { [] }
             | '[' Alfa_ran ']'                    { $2 }
             | ListaArreglo ',' ListaArreglo       { $1 ++ $3 }
             | '[' ListaArreglo ']'                { $2 }

Expr     : Asig                                    { $1 }
         | Func                                    { $1 }
         | Instr                                   { $1 }
         | OpConj                                  { $1 }

Asig     : id ':=' OpConj                          { Asignacion (Var (takeStr $1)) $3 }

Func     : miembro '(' Elemento ',' Conjunto ')'        { Miembro $3 $5 }
         | vacio '(' OpConj ')'                         { Vacio $3 }
         | subconjunto '(' Conjunto ',' Conjunto ')'    { SubConjunto $3 $5 }

Instr    : estado                                  { Estado }
         | olvidar todo                            { OlvidarTodo }
         | olvidar Lista_id                        { Olvidar $2 }
         | fin                                     { Fin }

OpConj   : Conjunto                                { $1 }
         | Universo                                { $1 }
         | Extension                               { $1 }
         | OpConj '+' OpConj                       { Union $1 $3 }
         | OpConj '*' OpConj                       { Interseccion $1 $3 }
         | OpConj '-' OpConj                       { Diferencia $1 $3}
         | '~' OpConj                              { Complemento $2 }
         | OpConj '%' OpConj                       { Cartesiano $1 $3 }
         | OpConj '!'                              { Partes $1 }
         | '(' OpConj ')'                          { $2 }

Universo : universo                                { UniversoT }
         | universo de id                          { UniversoDe (Var (takeStr $3)) }

Extension : '{' ListaArrId '|' LGenerador ',' LFiltro '}'    { ConjuntoExt $2 $4 $6 }

ListaArrId : id                                    { [] }
           | '[' Lista_id ']'                      { $2 }

LGenerador : Generador                             { [$1] }
           | LGenerador ',' Generador              { $1 ++ [$3] }

Generador : id '<-' id                             { Gen (Var (takeStr $1)) (Var (takeStr $3)) }

LFiltro : Filtro                                   { [$1] }
        | LFiltro ',' Filtro                       { $1 ++ [$3] }

Filtro : Elemento '==' Elemento                    { FilIgual $1 $3 }
       | Elemento '<' Elemento                     { FilMenor $1 $3 }
       | Elemento '>' Elemento                     { FilMayor $1 $3 }
       | mayuscula Elemento                        { FilMayuscula $2 }
       | letra Elemento                            { FilLetra $2 }
       | digito Elemento                           { FilDigito $2 }
       | simbolo Elemento                          { FilSimbolo $2 }
       | not Filtro                                { FilNot $2 }
       | '(' Filtro ')'                            { $2 }

Elemento : id                                      { Ident (Var (takeStr $1)) }
         | str                                     { Elem (takeStr $1) }

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
  concatTup
  
  Funci�n que concatena listas encapsuladas en tuplas.
-}
concatTup :: ([a], [b]) -- ^ Primera tupla
          -> ([a], [b]) -- ^ Segunda tupla
          -> ([a], [b]) -- ^ Tupla resultante
concatTup (x,y) (w,z) = (w++x, y++z)

takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s
}
