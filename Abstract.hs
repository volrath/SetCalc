module Abstract (
        AST (..),
        Symbol(..),
        Dominio(..),
        Conjunto(..),
        Expresion(..),
        Inst(..),
        Ext(..),
        Generador(..),
        Filtro(..),
        Univ(..),
        Elemento(..)
) where
import SetC

{-|
  TAD /AST/:
  Tipo abstracto que modela un árbol abstracto de sintáxis
  donde todos los nodos son expresiones del lenguaje de la
  calculadora /SetCalc/.
-}
data AST = Expr Expresion -- * Nodo expresión
         | Secuencia [Expresion] -- * Secuencia de expresiones
         deriving (Eq, Show)

{-|
-}
data Symbol = Symbol (Maybe Dominio, Maybe Conjunto)
            deriving (Eq,Show)

data Dominio = Dominio (SetC Elemento)
             deriving (Eq, Show)

data Conjunto = Conjunto (SetC Elemento)
              deriving (Eq, Show)

data Inst = Estado
          | OlvidarTodo
          | Olvidar [String]
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
	       | OpId String
               | Asignacion String Expresion
                 deriving (Eq,Show)

data Ext = ConjuntoExt (SetC Elemento) [Generador] [Filtro]
         deriving (Eq,Show)

data Generador = Gen String String
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
          | UniversoDe String
          deriving (Eq,Show)

data Elemento = Elem String
              | Ident String
              | Cto (SetC Elemento)
              | Lista [Elemento]
              | Rango Char Char
              deriving (Eq,Show)

