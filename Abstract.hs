module Abstract (
        AST (..),
        Symbol(..),
        Dominio(..),
        Conjunto(..),
        Var(..),
        Expresion(..),
        Inst(..),
        Ext(..),
        Generador(..),
        Filtro(..),
        Univ(..),
        Elemento(..)
) where
import SetC

data AST = Expr Expresion
         | Secuencia [Expresion]
         deriving (Eq, Show)

data Symbol = Symbol (Maybe Dominio, Maybe Conjunto)
            deriving (Eq,Show)

data Dominio = Dominio (SetC Elemento)
             deriving (Eq, Show)

data Conjunto = Conjunto (SetC Elemento) -- (Dominio)Var
              deriving (Eq, Show)

data Var = Var String
         deriving (Eq,Show,Ord)

data Inst = Estado
          | OlvidarTodo
          | Olvidar [Var]
          | Fin
          deriving (Eq,Show)

data Expresion = Union Expresion Expresion
               | Interseccion Expresion Expresion
               | Diferencia Expresion Expresion
               | Cartesiano Expresion Expresion
               | Complemento Expresion
               | Partes Expresion
               | OpUniverso Univ
               | OpExtension Ext
               | OpConj Conjunto -- Symbol
	       | OpId Var
               | Asignacion Var Expresion
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
            | Miembro Elemento Conjunto
            | Vacio Expresion
            | SubConjunto Conjunto Conjunto
            deriving (Eq,Show)

data Univ = UniversoT Conjunto
          | UniversoDe Var
          deriving (Eq,Show)

data Elemento = Elem String
              | Ident Var
              | Cto (SetC Elemento)
              | Lista [Elemento]
              | Rango Char Char
              deriving (Eq,Show)


-- parser $ lexer "foo es dominio {['a','b'], ['c','d']}."