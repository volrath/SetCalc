module Abstract (
        AST (..),
        Symbol(..),
        Var(..),
        Expresion(..),
        Funcion(..),
        Inst(..),
        Op(..),
        Ext(..),
        Generador(..),
        Filtro(..),
        Univ(..),
        AlfaRan(..),
        Elemento(..)
) where

data AST = Expresion
         | Secuencia AST Expresion
         deriving (Eq,Show)

data Symbol = Conjunto [AlfaRan]
            | Dominio [Elemento]
            deriving (Eq,Show)

data Var = Var String
         deriving (Eq,Show)

data Expresion = Asignacion Var Op
               | Funcion Funcion
               | Instruccion Inst
               | Operacion Op
               deriving (Eq,Show)

data Funcion = Miembro Elemento Symbol
             | Vacio Op
             | SubConjunto Symbol Symbol
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
        | Universo Univ
        | Extension Ext
        | Conj Symbol
	| Id Var
        deriving (Eq,Show)

data Ext = ConjuntoExt [Var] [Generador] [Filtro]
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
            deriving (Eq,Show)

data Univ = UniversoT
          | UniversoDe Var
          deriving (Eq,Show)

data AlfaRan = Rango Char Char
             | Elemento Elemento
             deriving (Eq,Show)

data Elemento = Elem String
              | Ident Var
              deriving (Eq,Show)
