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
        Factor(..),
        AlfaRan(..),
        Elemento(..)
) where

data AST = Expresion
         | Secuencia AST Expresion

data Symbol = Conjunto
            | Dominio

data Var = Var String

data Expresion = Asignacion Var Op
               | Funcion Funcion
               | Instruccion Inst
               | Operacion Op

data Funcion = Miembro Elemento Symbol
             | Vacio Op
             | SubConjunto Symbol Symbol

data Inst = Estado
          | OlvidarTodo
          | Olvidar [Var]
          | Fin

data Op = Union Op Op
        | Interseccion Op Op
        | Diferencia Op Op
        | Cartesiano Op Op
        | Complemento Op
        | Partes Op
        | Universo Univ
        | Extension Ext
        | Factor Factor

data Ext = ConjuntoExt [Var] [Generador] [Filtro]

data Generador = Gen Var Var
data Filtro = FilIgual Elemento Elemento
            | FilMenor Elemento Elemento
            | FilMayor Elemento Elemento
            | FilMayuscula Elemento
            | FilLetra Elemento
            | FilDigito Elemento
            | FilSimbolo Elemento
            | FilNot Filtro

data Univ = UniversoT
          | UniversoDe Var

data Factor = Op Op
            | Symbol
            | Id Var

data AlfaRan = Rango Char Char
             | Elemento

data Elemento = Elem String
              | Ident Var