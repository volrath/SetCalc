{-|
  /Tipo Abstracto de Datos SetC/
  
  Traductores e Interpretadores CI-3725

  Abril-Julio 2008

  Kristoffer Pantic <sktdude@gmail.com>     05-38675

  Daniel Barreto    <daniel@ac.labf.usb.ve> 04-36723
  
  Representación del TAD SetC que modelará la máquina
  de conjuntos a utilizar para ejecutar las instrucciones del
  lenguaje @SetCalc@ a implementar en un futuro. SetC esta basado
  en el uso de listas de Haskell para representar los conjuntos.
-}
module SetC (
   -- * Tipo Abstracto
   SetC,
   -- * Funciones exportadas
   emptySet,
   singletonSet,
   isEmpty,
   isMember,
   subSet,
   properSubSet,
   addToSet,
   unionSet,
   intersectSet,
   minusSet,
   fromList,
   toList,
   powerSet,
   takeType,
   mapSet
) where

import List

instance Show a => Show (SetC a) where
  show (Set s) = "{" ++ showSetC s
    where
      showSetC []       = "}"
      showSetC [x]      = (show x) ++ "}"
      showSetC (x:y:xs) = (show x) ++ ", " ++ showSetC (y:xs) 
 
instance Eq a => Eq (SetC a) where
  (Set b) == (Set a) = (subSet (Set b) (Set a)) && (subSet (Set a) (Set b))

{-|
  El tipo de datos @SetC a@ modela los conjuntos matemáticos cuyos
  elementos son del tipo @a@
-}
newtype SetC a = Set [a]


{-|
  @emptySet@
  
  Retorna un conjunto vacío
-}
emptySet :: SetC a
emptySet = Set []


{-|
  @singletonSet element@
  
  Retorna un conjunto cuyo único elemento es @element@
-}
singletonSet :: a -- ^ Elemento que llevará el conjunto a retornar
             -> SetC a -- ^ Conjunto resultante
singletonSet a = Set [a]


{-|
  @isEmpty set@
  
  Retorna un valor booleano que indica si el conjunto @set@ está o no vacío
-}
isEmpty :: SetC a -- ^ Conjunto del cual se quiere saber su estado de vacuidad
        -> Bool -- ^ True si el conjunto está vacío, False si lo contrario
isEmpty (Set a) = null a


{-|
  @isMember element set@
  
  Retorna un valor booleano que indica si el elemento @element@ pertenece al
  conjunto @set@
-}
isMember :: (Eq a) => a -- ^ Elemento a verificar su pertenencia en el conjunto
         -> SetC a -- ^ Conjunto sobre el cual se buscara al elemento
         -> Bool  -- ^ True si el elemento esta en el conjunto, False si lo contrario
isMember e (Set x) = elem e x


{-|
  @subSet set1 set2@
  
  Retorna un valor booleano que indica si el conjunto @set1@ es subconjunto del
  conjunto @set2@
-}
subSet :: (Eq a) => 
          SetC a -- ^ Conjunto
       -> SetC a -- ^ Conjunto
       -> Bool -- ^ True si el primer conjunto es subconjunto del segundo, False si lo contrario
subSet (Set []) _ = True
subSet (Set [a]) (Set b) = elem a b
subSet (Set (x:xs)) (Set b) = (elem x b) && (subSet (Set xs) (Set b))


{-|
  @properSubSet set1 set2@
  
  Retorna un valor booleano que indica si el conjunto @set1@ es subconjunto
  propio del conjunto @set2@
-}
properSubSet :: (Eq a) => SetC a -- ^ Conjunto
             -> SetC a -- ^ Conjunto
             -> Bool -- ^ True si el primer conjunto es subconjunto propio del segundo, False si lo contrario
properSubSet (Set a) (Set b) = (subSet (Set a) (Set b)) && (not $ subSet (Set b) (Set a))


{-|
  @addToSet element set@
  
  Agrega el elemento @element@ al conjunto @set@, tomando encuenta que si el
  elemento ya se encuentra en el conjunto, no es necesario agregarlo.
-}
addToSet :: (Eq a) => a -- ^ Elemento a agregar
         -> SetC a -- ^ Conjunto original
         -> SetC a -- ^ Conjunto resultante
addToSet e (Set x)
         | elem e x = Set x
         | otherwise = Set (e:x)


{-|
  @unionSet set1 set2@
  
  Calcula la unión de los conjuntos @set1@ y @set2@
-}
unionSet :: (Eq a) => SetC a -- ^ Conjunto
         -> SetC a -- ^ Conjunto
         -> SetC a -- ^ Conjunto resultante
unionSet (Set x) (Set y) = Set (union x y)


{-|
  @fromList list@
  
  Calcula un conjunto a partir de los elementos de una lista @list@.
-}
fromList :: (Eq a) => [a] -- ^ Lista de donde se sacarán los elementos del nuevo conjunto
         -> SetC a -- ^ Conjunto resultante
fromList x = Set (nub x)


{-|
  @powerSet set@
  
  Calcula el conjunto de las partes del conjunto @set@.
-}
powerSet :: (Eq a) => SetC a -- ^ Conjunto a obtener el conjunto de sus partes
         -> SetC (SetC a) -- ^ Conjunto de conjuntos resultante
powerSet (Set []) = (Set [Set []])
powerSet (Set(x:xs)) = unionSet z (mapSet (addToSet x) z) where
                                                          z = powerSet (Set xs)


{-|
  @mapSet function set@
  
  Aplica una función @function@ a todos los elementos del conjunto @set@.
-}
mapSet :: (a -> b) -- ^ Función a aplicar a los elementos del conjunto
       -> SetC a -- ^ Conjunto
       -> SetC b -- ^ Conjunto resultante
mapSet fun (Set c) = Set (map fun c)


{-|
  @intersectSet set1 set2@
  
  Calcula la intersección de los conjuntos @set1@ y @set2@.
-}
intersectSet :: (Eq a) => SetC a -- ^ Conjunto
             -> SetC a -- ^ Conjunto
             -> SetC a -- ^ Conjunto resultante
intersectSet (Set x) (Set y) = Set (intersect x y)


{-|
  @minusSet set1 set2@
  
  Calcula la diferencia de los conjuntos @set1@ y @set2@.
-}
minusSet :: (Eq a) => SetC a -- ^ Conjunto
         -> SetC a -- ^ Conjunto
         -> SetC a -- ^ Conjunto resultante
minusSet (Set x) (Set y) = Set (x \\ y)

takeType :: SetC a
         -> Maybe [a]
takeType (Set []) = Nothing
takeType (Set l@(x:xs))  = Just l

{-|
  @toList set@
  
  Recibe un conjunto @set@ y retorna una lista compuesta por los elementos del conjunto.
-}
toList :: SetC a -- ^ Conjunto del que se usarán sus elementos
       -> [a] -- ^ Lista resultante
toList (Set x) = x

