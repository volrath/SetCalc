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
import Lexer

{-|
  TAD /AST/:
  Tipo abstracto que modela un árbol abstracto de sintáxis
  donde todos los nodos son expresiones del lenguaje de la
  calculadora /SetCalc/.
-}
data AST = Expr Expresion -- ^ Nodo expresión
         | Secuencia [Expresion] -- ^ Secuencia de expresiones
         deriving (Eq, Show)

{-|
  TAD /Symbol/:
  Tipo abstracto de datos que modela un símbolo del lenguaje
  de la calculadora SetCalc.
-}
data Symbol = Symbol (Maybe Dominio, Maybe Conjunto) -- ^ Un símbolo puede ser un Dominio, un Conjunto o ambos.
            deriving (Eq,Show)

{-|
  TAD /Dominio/:
  Tipo abstracto de datos que modela un dominio del lenguaje
  de calculadora /SetCalc/
-}
data Dominio = Dominio (SetC Elemento) -- ^ Dominio
             | DominioID String
             deriving (Eq, Show)

{-|
  TAD /Conjunto/:
  Tipo abstracto de datos que modela un cojunto del lenguaje
  de calculadora /SetCalc/
-}
data Conjunto = Conjunto (SetC Elemento) Dominio -- ^ Conjunto
              deriving (Eq, Show)

{-|
  TAD /Inst/:
  Tipo abstracto de datos que modela una instrucción especial
  del lenguaje de calculadora /SetCalc/
-}
data Inst = Estado -- ^ La instrucción "estado"
          | OlvidarTodo -- ^ La instrucción "olvidar todo"
          | Olvidar [Token] -- ^ La instrucción "olvidar var"
          | Fin -- ^ La instrucción "fin"
          deriving (Eq,Show)

{-|
  TAD /Expresion/:
  Tipo abstracto de datos que modela una expresión del lenguaje
  de calculadora /SetCalc/
-}
data Expresion = Union Expresion Expresion -- ^ Unión de dos conjuntos
               | Interseccion Expresion Expresion -- ^ Intersección de dos conjuntos
               | Diferencia Expresion Expresion -- ^ Diferencia de dos conjuntos
               | Cartesiano Expresion Expresion -- ^ Producto cartesiano de dos conjuntos
               | Complemento Expresion -- ^ Complemento de un conjunto.
               | Partes Expresion -- ^ Conjunto de partes de un conjunto.
               | OpUniverso Univ -- ^ Expresión que genera un universo de instancias.
               | OpExtension Ext -- ^ Conjunto definido por extensión.
               | OpConj Conjunto -- ^ Conjunto.
	       | OpId Token -- ^ Identificador de algún conjunto.
               | Asignacion Token Expresion -- ^ Asignación de algún conjunto a una variable
                 deriving (Eq,Show)
{- instance Show Expresion where
    show (Union e1 e2) = (show e1) ++ " + " ++ (show e2)
    show (Interseccion e1 e2) = (show e1) ++ " * " ++ (show e2)
    show (Diferencia e1 e2) = (show e1) ++ " - " ++ (show e2)
    show (Cartesiano e1 e2) = (show e1) ++ " x " ++ (show e2)
    show (Complemento e) = "~" ++ (show e)
    show (Partes e) = (show e) ++ "!"
    show (OpUniverso (UniversoT c)) = (show c)
    show (OpUniverso (UniversoDe t)) = "Universo de " ++ (show $ takeStr t)
    show (OpExtension e) = (show e)
    show (OpConj c) = (show c)
    show (OpId t) = (show $ takeStr t)
    show (Asignacion t e) = (show $ takeStr t) ++ " := " ++ (show e)
-}
{-|
  TAD /Ext/:
  Tipo abstracto de datos que modela un conjunto definido por extensión
-}
data Ext = ConjuntoExt (SetC Elemento) [Generador] [Filtro] -- ^ Conjunto definido por extensión.
         deriving (Eq)
instance Show Ext where
    show (ConjuntoExt c gs fs) = "{" ++ (show c) ++ " | " ++ (show gs) ++ ", " ++ (show fs) ++ "}"

{-|
  TAD /Generador/:
  Tipo abstracto de datos que modela un generador de un conjunto definido
  por extensión en el lenguaje de calculadora /SetCalc/.
-}
data Generador = Gen String Token -- ^ Generador.
               deriving (Eq)
instance Show Generador where
    show (Gen s t) = (show s) ++ " <- " ++ (show $ takeStr t)

-- newtype ListaGeneradores = LG [Generador] deriving (Eq)
-- instance Show ListaGeneradores where
--     show (LG (g:[])) = (show g)
--     show (LG (g:gs)) = (show g) ++ ", " ++ (show gs)

{-|
  TAD /Filtro/:
  Tipo abstracto de datos que modela un filtro de un conjunto definido
  por extensión en el lenguaje de calculadora /SetCalc/.
-}
data Filtro = FilIgual Elemento Elemento -- ^ Igualdad.
            | FilMenor Elemento Elemento -- ^ Menor que.
            | FilMayor Elemento Elemento -- ^ Mayor que.
            | FilMayuscula Elemento -- ^ Mayúscula.
            | FilLetra Elemento -- ^ Letra.
            | FilDigito Elemento -- ^ Digito.
            | FilSimbolo Elemento -- ^ Símbolo.
            | FilNot Filtro -- ^ Not
            | Miembro Elemento Conjunto -- ^ Miembro.
            | Vacio Expresion -- ^ Vacío
            | SubConjunto Conjunto Conjunto -- ^ Sumconjunto
            deriving (Eq,Show)

{-|
  TAD /Univ/:
  Tipo abstracto de datos que modela un conjunto que posee el universo
  de instancias del lenguaje o el universo de una variable en particular.
-}
data Univ = UniversoT Conjunto -- ^ Universo de todos los caracteres imprimibles en haskell.
          | UniversoDe Token -- ^ Universo de todas las instancias de una variable específica.
          deriving (Eq,Show)

{-|
  TAD /Elemento/:
  Tipo abstracto de datos que modela los elementos que existen en el
  lenguaje de calculadora /SetCalc/.
-}
data Elemento = Elem String -- ^ Un elemento cualquiera.
              | Ident Token -- ^ Un identificador.
              | Cto (SetC Elemento) -- ^ Un conjunto de elementos.
              | Lista [Elemento] -- ^ Una lista de elementos.
              | Rango Char Char -- ^ Un rango de elementos.
              deriving (Eq)
instance Show Elemento where
    show (Elem s) = show s
    show (Ident t) = "Var(" ++ (show $ takeStr t) ++ ")"
    show (Cto c) = show c
    show (Lista es) = show es
    show (Rango c1 c2) = (show c1) ++ ".." ++ (show c2)

{-|
  Función que devuelve el string envuelto por un Token cuyo constructor
  sea TkStr o TkId.
-}
takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s
