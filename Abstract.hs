module Abstract (
        AST (..),
        Symbol(..),
        Var(..),
        Expresion(..),
        Inst(..),
        Op(..),
        Ext(..),
        Generador(..),
        Filtro(..),
        Univ(..),
        Elemento(..)
) where
import SetC

data AST = Expr Expresion
         | Secuencia [Expresion]
              deriving (Eq,Show)

data Symbol = Conjunto (SetC Elemento)
            | Dominio (SetC Elemento)
            deriving (Eq,Show)

data Var = Var String
         deriving (Eq,Show,Ord)

data Expresion = Instruccion Inst
               | Operacion Op
               deriving (Eq,Show)

data Inst = Estado
          | OlvidarTodo
          | Olvidar [Var]
          | Fin
          deriving (Eq,Show)

data Op = Union Op Op
        | Interseccion Op Op
        | Diferencia Op Op
        | Cartesiano Op Op
        | Complemento Op
        | Partes Op
        | OpUniverso Univ
        | OpExtension Ext
        | OpConj Symbol
	| OpId Var
        | Asignacion Var Op
        deriving (Eq,Show)

data Ext = ConjuntoExt (SetC Elemento) [Generador] [Filtro]
         deriving (Eq,Show)

data Generador = Gen Var Var
               deriving (Eq,Show)

data Filtro = FilIgual Elemento Elemento
            | FilMenor Elemento Elemento
            | FilMayor Elemento Elemento
            | FilMayuscula Elemento
            | FilLetra Elemento
            | FilDigito Elemento
            | FilSimbolo Elemento
            | FilNot Filtro
            | Miembro Elemento Symbol
            | Vacio Op
            | SubConjunto Symbol Symbol
            deriving (Eq,Show)

data Univ = UniversoT Symbol
          | UniversoDe Var
          deriving (Eq,Show)

data Elemento = Elem String
              | Ident Var
              | Cto (SetC Elemento)
              | Lista [Elemento]
              | Rango Char Char
              deriving (Eq,Show)


-- parser $ lexer "foo es dominio {['a','b'], ['c','d']}."