{-|
   /Programa Principal - Calculadora/
   
   Traductores e Interpretadores CI3725
   

   05-38675 Kristoffer Pantic <sktdude@gmail.com>

   04-36723 Daniel Barreto N. <daniel@ac.labf.usb.ve>
   
   Programa Principal para el interpretador de la calculadora.

 -}
module Main (
-- * Función Principal.
  main, chequear
) where
import SetC
import System.IO
import System.Exit
import System(getArgs)
import qualified Control.Exception as C
import qualified Data.Map as Map
import Lexer
import Parser
import Interpreter
import Abstract

type SymTable = Map.Map String Symbol
type SetTable = Map.Map String (SetC Elemento)
type TupParser = (Map.Map String Symbol, AST)

{-|
   Función principal.

   El programa puede ser compilado para su ejecución directa,
   o bien cargado en el interpretador GHCi e invocado a través
   de la función @main@.
 -}
main :: IO ()
main =
    do
      args <- getArgs
      if null args 
         then do
           -- Se corre el interpretador
           putStr "Interpretador SetCalc:\n"
           loop Map.empty
         else do
           if (length args) == 1
             then do
               -- Se abre el archivo y se analiza
               content <- readFileOrCatch $ head args
               case content of
                 Right parserResult -> print parserResult
                 Left err -> do
                              hPutStr stderr $ "Imposible abrir el archivo " ++ (head args) ++ " debido a:"
                              hPrint stderr err
             else do hPutStr stderr $ "Modo de uso: ./SetCalc [archivo] -- Un unico archivo.\n"

--
-- Funciones auxiliares
--
-- Para el interpretador como consola
--
{-|
  promptAndGet

  Función que se encarga de mostrar un prompt en el interpretador
  de la calculadora SetCalc y luego de retornar la línea
  introducida por el usuario.
-}
promptAndGet :: IO String -- ^ Línea leída desde la consola
promptAndGet = do
    hSetBuffering stdout NoBuffering
    putStr "SetCalc> "
    >> getLine

loop :: Map.Map String Symbol 
     -> IO()

loop mapaActual = do
  line <- promptAndGet
  tp <- return $ procesarLinea mapaActual line
  catchSilently tp mapaActual


{-|
  catchOrPrint

  Función que recibe la ejecucion del parser y el lexer sobre un
  string, trata de imprimir dicha instruccion pero si hubo errores
  los imprime por la salida de error estandar y permite recuperar
  el prompt.
-}
catchSilently ::  TupParser -- ^ La ejecución de las funciones parser y lexer sobre un string.
              ->  Map.Map String Symbol 
              -> IO() -- ^ La impresión del resultado de la ejecución de la función.
catchSilently tup old = C.catch try (fail old)
    where 
      try = do
        interpreter tup
        loop $ fst $ tup
      fail old e = hPrint stderr e >> (loop old)



procesarLinea :: SymTable
              -> String
              -> TupParser
procesarLinea mapa linea = case chequeoEstructural elTupParser of
                             Nothing -> ((chequeoDinamico elTupParser), (snd elTupParser))
                             Just errs -> error $ errs
    where elTupParser = chequear mapa (lexer linea)



--
-- Para el interpretador de archivos
--
{-|
  readFileOrCatch
  
  Función que lee un archivo @fpath@ y devuelve la Tupla del
  Data.Map y el AST generados al analizar sintácticamente el
  archivo o una excepción de no haber podido abrirlo.
-}
readFileOrCatch :: FilePath -- ^ Archivo a abrir.
                -> IO (Either C.IOException TupParser) -- ^ Si no hubo fallos, devuelve la tupla (Data.Map, AST) generada por el analizador sintáctico, en otro caso devuelve una Excepción
readFileOrCatch fpath = catch (try fpath) fail
    where
      try f = do
        c <- readFile f
        let parserResult = parser $ lexer c
        return $ Right parserResult
      fail e = return (Left e)

chequear :: Map.Map String Symbol
         -> [Token]
         -> TupParser

chequear map a = chequeo $ parser $ a
    where
      chequeo f = case revisarErrores map (Right f) of
                   Right (m,t) -> existenErrores (m,t) (Map.toList m)
                   Left err -> error $ err

revisarErrores :: (Map.Map String Symbol)
               -> Either (TupParser,String) TupParser
               -> Either String TupParser
revisarErrores map map' = case map' of
                            Right (map1,Secuencia (x:[])) -> case revisarErrores' (map) (Right (map1,Secuencia [x])) of
                                                               Right (map3,ast) -> Right (map3,ast)
                                                               Left (map3,err) -> Left err
                            Right (map1,Secuencia (x:xs)) -> case revisarErrores' (map) (Right (map1,Secuencia [x])) of
                                                               Right (mapb,a) -> revisarErrores mapb (Right (Map.empty,Secuencia xs))
                                                               Left (map3,err) -> revisarErrores map (Left ((map1,Secuencia xs),err))
                            Right (map1,Secuencia []) -> case revisarErrores' (map) (Right (map1,Secuencia [])) of 
                                                           Right (map3,ast) -> Right (map3,ast)
                                                           Left (map3,err) -> Left err
                            Left ((maperr,asterr), err) -> case revisarErrores' map (Left ((maperr,asterr),err)) of
                                                             Right _ -> error $ "Error 0x123ABCF2"
                                                             Left (map1,err2) -> Left (err2)
                                            
revisarErrores' :: (Map.Map String Symbol) -- ^ Tupla que puede ser TupParser en caso de no haber errores o en caso de haber errores de contexto,una tupla que contiene el Data.Map generado hasta el momento y un string con todos los errores concatenados.
            -> Either (TupParser,String) TupParser -- ^ Es TupParser en caso de que la expresion o declaración analizada no tenga errores de contexto o un String en caso contrario.
            -> Either ((Map.Map String Symbol), String) TupParser -- ^ Tupla que puede ser TupParser en caso de no haber errores o en caso de haber errores de contexto,una tupla que contiene el Data.Map generado hasta el momento y un string con todos los errores concatenados.

revisarErrores' m tup2 = case tup2 of 
                              Right (n, b) -> case Map.null (chequearAsignacion b) of 
                                                True -> case unirMapas m n of
                                                          Right res -> Right (res, b)
                                                          Left err2 -> Left (m,err2)
                                                False -> case unirMapas m n of
                                                           Right res -> case actualizarMapa res (Map.toList (chequearAsignacion b)) of
                                                                          Right res2 -> Right (res2, b)
                                                                          Left (m1,err2) -> Left (m,err2)
                                                           Left err -> case actualizarMapa m (Map.toList (chequearAsignacion b)) of
                                                                              Right res2 -> Left (res2, err)
                                                                              Left (m1,err2) -> Left (m, err ++ "\n" ++ err2)
                              Left ((n,a),errs)  -> case Map.null (chequearAsignacion a) of 
                                                      True -> case unirMapas m n of 
                                                                Right res -> Left (m,errs)
                                                                Left err2 -> Left (m,errs ++"\n" ++ err2)
                                                      False -> case unirMapas m n of
                                                                 Right res -> case actualizarMapa res (Map.toList (chequearAsignacion a)) of
                                                                                Right res2 -> Left (m, errs)
                                                                                Left (m1,err2) -> Left (m,errs ++ "\n" ++ err2)
                                                                 Left err -> case actualizarMapa m (Map.toList (chequearAsignacion a)) of
                                                                               Right res2 -> Left (m, err ++ "\n" ++ errs)
                                                                               Left (m1,err2) -> Left (m, errs ++ "\n" ++ err ++ "\n" ++ err2)

{-|
                La función @construirAST@ concatena dos árboles sintácticos
                AST generados por el analizador lexicográfico.
-}
                                               
construirAST :: AST -- ^ Árbol sintáctico a ser concatenado.
             -> AST -- ^ Árbol sintáctico a ser concatenado.
             -> AST -- ^ Árbol sintáctico que resulta de la concatenación de los otros AST.

construirAST (Expr a) (Expr b) = Secuencia [a, b]
construirAST (Expr a) (Secuencia b) = Secuencia ([a] ++ b)
construirAST (Secuencia a) (Expr b) = Secuencia (a ++ [b])
construirAST (Secuencia a) (Secuencia b) = Secuencia (a ++ b)

{-|
                La función @unirMapas@ realiza la unión de dos mapas
                de símbolos mientras revisa si hay errores de contexto.
-}

unirMapas :: Map.Map String Symbol -- ^ Mapa de símbolos a ser unido.
          -> Map.Map String Symbol -- ^ Mapa de símbolos a ser unido.
          -> Either String (Map.Map String Symbol) -- ^ Devuelve el resultado de la unión de los dos mapas de símbolos en caso de no haber errores y un String con los errores en caso contrario.

unirMapas map1 map2 = unirMapas' map1 (Map.toList map2)

{-|
                La función @unirMapas'@ realiza la inserción
                de la lista de tuplas conformadas por variables y símbolos
                en un mapa de símbolos revisando si hay errores de contexto.
-}

unirMapas' :: Map.Map String Symbol -- ^ Mapa de símbolos a la que se van a insertar los valores.
           -> [(String, Symbol)] -- ^ Lista de tuplas formadas por variables y símbolos que será insertada en el mapa en caso de no haber errores de contexto.
           -> Either String (Map.Map String Symbol) -- ^ Devuelve el resultado de la inserción de los valores en el mapa en caso de no haber errores, en caso contrario se devuelve un String con los errores.

unirMapas' map1 [] = Right map1
unirMapas' map1 (x:[]) = crearValor map1 x
unirMapas' map1 (x:xs) = case mapR of
                           Right map2 -> case unirMapas' map2 xs of
                                          Right m -> Right (Map.union map2 m)
                                          Left errs -> Left errs
                           Left err -> case unirMapas' map1 xs of
                                         Right m -> Left err
                                         Left errs -> Left (errs ++ err)
    where
      mapR = crearValor map1 x

{-|
                La función @crearValor@ decide, en función del valor introducido
                si se va a generar una entrada en el mapa para la variable
                como Conjunto o como Dominio y los genera e inserta en el mapa en caso de no haber
                errores de contexto y en caso contrario devuelve un String con
                los errores.
-}

crearValor :: Map.Map String Symbol -- ^ Mapa de símbolos donde se va a insertar el valor.
           -> (String, Symbol) -- ^ Tupla que puede tener un valor de Conjunto o de Dominio y que se va a intentar insertar en el mapa.
           -> Either String (Map.Map String Symbol) -- ^ Devuelve un String en caso de haber errores y un mapa de símbolos con el valor nuevo introducido en caso contrario.

crearValor map x = case snd(x) of
                     Symbol(Just dom, Nothing) -> crearDominio map (fst(x),Symbol(Just dom, Nothing))
                     Symbol(Nothing, Just conj) -> crearConjunto map (fst(x),Symbol(Nothing, Just conj))
                     Symbol(Just dom, Just conj) -> crearSymbol map (fst(x),Symbol(Just dom, Just conj))
                     _ -> Left "Error 0x08042FF2"


existenErrores :: TupParser
               -> [(String,Symbol)]
               -> TupParser

existenErrores t xs = case existeSymbol t xs of
                         Right t -> t
                         Left err -> error $ err

existeSymbol :: TupParser
             -> [(String,Symbol)]
             -> Either String TupParser
existeSymbol tp [] = Right tp
existeSymbol (map,ast) (x:[]) = case existeSymbol' map x of
                                  Right m -> Right (m,ast)
                                  Left err -> Left err
existeSymbol (map,ast) (x:xs) = case existeSymbol' map x of
                                  Right m -> existeSymbol (m,ast) xs
                                  Left err -> case existeSymbol (map,ast) xs of
                                                Right m1 -> Left err
                                                Left err2 -> Left (err ++ "\n" ++ err2)

existeSymbol' :: Map.Map String Symbol
             -> (String,Symbol)
             -> Either String (Map.Map String Symbol)

existeSymbol' map (st,sy) = case sy of
                       Symbol(Just (DominioID dom), Nothing) -> case Map.lookup dom map of
                                                                  Just (Symbol(Just dom2, _)) -> Right map
                                                                  _ -> Left ("No existe la variable " ++ dom ++ " definida como dominio")
                       Symbol(Nothing, Just (Conjunto con (DominioID dom))) -> case Map.lookup dom map of
                                                                                 Just (Symbol(Just dom2, _)) -> Right map
                                                                                 _ -> Left ("No existe la variable " ++ dom ++ " definida como dominio")
                       Symbol(Just (DominioID dom1), Just (Conjunto con (DominioID dom2))) -> if dom2 == st 
                                                                                                 then case Map.lookup dom1 map of
                                                                                                       Just (Symbol(Just dom2, _)) -> Right map
                                                                                                       _ -> Left ("No existe la variable " ++ dom1 ++ " definida como dominio")           
                                                                                                 else case Map.lookup dom1 map of
                                                                                                        Just (Symbol(Just dom, _)) -> case Map.lookup dom2 map of 
                                                                                                                                        Just (Symbol(Just dom3,_)) -> Right map
                                                                                                                                        _ -> Left ("No existe la variable "++ dom2 ++" definida como dominio") 
                                                                                                        Nothing -> case Map.lookup dom2 map of
                                                                                                                     Just (Symbol(Just dom3,_)) -> Left ("No existe la variable " ++ dom1 ++ " definida como dominio")
                                                                                                                     _ -> Left ("No existe la variable " ++ dom1 ++ " definida como dominio \n No existe la variable "++ dom2 ++" definida como dominio")                                                                           
                       _ -> Right map
 
{-|
                La función @crearDominio@ intenta insertar en un mapa de símbolos
                una nueva entrada para una variable en su definición como Dominio,
                en caso de ya existir este valor se genera un error de contexto y en
                caso contrario se devuelve el mapa resultante. 
-}

crearSymbol :: Map.Map String Symbol -- ^ Mapa donde se va a intentar insertar el valor.
             -> (String, Symbol) -- ^ Entrada que se intentará insertar en el mapa de símbolos.
             -> Either String (Map.Map String Symbol) -- ^ Devuelve el mapa resultante en caso de no haber errores de contexto y un String con el error en caso contrario.

crearSymbol map (k, v) = case  Map.lookup (k) (map) of
      Just (Symbol (Just dom, Just con)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un dominio con el nombre de variable " ++ k ++ ".\n")
      Just (Symbol (Just dom, Nothing)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un dominio con el nombre de variable " ++ k ++ ".\n")
      Just (Symbol (Nothing, Just con)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un conjunto con el nombre de variable " ++ k ++ ".\n")
      Nothing -> Right (Map.insert k v map)
                      
{-|
                La función @crearDominio@ intenta insertar en un mapa de símbolos
                una nueva entrada para una variable en su definición como Dominio,
                en caso de ya existir este valor se genera un error de contexto y en
                caso contrario se devuelve el mapa resultante.
-}

crearDominio :: Map.Map String Symbol -- ^ Mapa donde se va a intentar insertar el valor.
             -> (String, Symbol) -- ^ Entrada que se intentará insertar en el mapa de símbolos.
             -> Either String (Map.Map String Symbol) -- ^ Devuelve el mapa resultante en caso de no haber errores de contexto y un String con el error en caso contrario.

crearDominio map (k, v) = case  Map.lookup (k) (map) of
      Just (Symbol (Just dom, Just con)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un dominio con el nombre de variable " ++ k ++ ".\n")
      Just (Symbol (Just dom, Nothing)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un dominio con el nombre de variable " ++ k ++ ".\n")
      Just (Symbol (Nothing, Just con)) -> Right (Map.union (Map.singleton k (Symbol (Just (takeDom v), Just con))) map)
      Nothing -> Right (Map.insert k (Symbol (Just (takeDom v), Nothing)) map)

{-|
                La función @crearConjunto@ intenta insertar en un mapa de símbolos
                una nueva entrada para una variable en su definición como Conjunto,
                en caso de ya existir este valor se genera un error de contexto y en
                caso contrario se devuelve el mapa resultante.
-}

crearConjunto :: Map.Map String Symbol -- ^ Mapa donde se va a intentar insertar el valor.
              -> (String, Symbol) -- ^ Entrada que intentará insertar en el mapa de símbolos.
              -> Either String (Map.Map String Symbol) -- ^ Devuelve el mapa resultante en caso de no haber errores de contexto y un String con el error en caso contrario.

crearConjunto map (k, v) = case  Map.lookup (k) (map) of
      Just (Symbol (Just dom, Just con)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un conjunto con el nombre de variable " ++ k ++ ".\n")
      Just (Symbol (Nothing, Just con)) -> Left ("Existe una doble declaracion en la linea , en la columna , ya existe un conjunto con el nombre de variable " ++ k ++ ".\n")
      Just (Symbol (Just dom, Nothing)) -> Right (Map.union (Map.singleton k (Symbol (Just dom , Just (takeConj v)))) map)
      Nothing -> Right (Map.insert k (Symbol (Nothing , Just (takeConj v))) map)

{-|
  La función @actualizarMapa@ recibe un Mapa de símbolos
  y una lista de tuplas de entradas que representan 
  las variables usadas en las asignaciones y en los generadores
  que se encuentran en las expresiones del programa
  y mediante el uso de la función actualizarMapa' se
  genera un error en caso de haberse usado una variable no
  definida y se devuelve el mapa resultante en caso contrario.
-}
actualizarMapa :: Map.Map String Symbol -- ^ Mapa generado por las declaraciones del programa hasta el momento.
               -> [(String, Symbol)]  -- ^ Lista de tuplas que fueron usadas en asignaciones y en generadores.
               -> Either ((Map.Map String Symbol), String) (Map.Map String Symbol) -- ^ Devuelve una tupla con el mapa generado hasta el momento con las asignaciones que han sido aceptadas acompañada de un String con los errores en caso de haber errores de contexto, en caso contrario devuelve el mapa resultante de realizar las asignaciones.
actualizarMapa map1 ((m2key, m2value):[]) = case actualizarMapa' m2key map1 of
                                             Right res -> Right res
                                             Left err -> Left (map1,err)
actualizarMapa map1 ((m2key, m2value):m2s) = case actualizarMapa' m2key map1 of
                                              Right res -> case actualizarMapa map1 m2s of 
                                                            Right res1 -> Right (Map.union res res1)
                                                            Left (m,err) -> Left (Map.union res m, err)
                                              Left err -> case actualizarMapa map1 m2s of
                                                           Right res1 -> Left (res1,err)
                                                           Left (m,err1) -> Left (m,err++err1)

{-|
  Recibe una variable y un mapa de símbolos y revisa si dicha
  variable se encuentra definida en el mapa como un conjunto.
  Si la encuentra devuelve el mapa de símbolos (actualizado
  para la siguiente entrega), pero si no lo encuentra devuelve
  un error correspondiente.
-}
actualizarMapa' :: String -- ^ Variable a chequear
                -> Map.Map String Symbol -- ^ Mapa en donde se buscará la variable
                -> Either String (Map.Map String Symbol)-- ^ Si se encontró la variable se devuelve el mapa actualizado. Error en caso contrario.
actualizarMapa' key map = case Map.lookup (key) (map) of
                           Just (Symbol (_, Just con)) -> Right map
                           Just (Symbol (_, Nothing)) -> Left ("La variable " ++ key ++ " no esta definida como conjunto y es usada en la linea y en la columna. \n")
                           Nothing -> Left ("La variable " ++ key ++ " no esta definida como conjunto y es usada en la linea y en la columna. \n")
      
chequearAsignacion :: AST -- ^ AST a analizar
                   -> Map.Map String Symbol -- ^ Mapa de símbolos resultante
chequearAsignacion (Secuencia []) = Map.empty
chequearAsignacion (Expr exp) = chequearAsignacion' exp Map.empty
chequearAsignacion (Secuencia (x:[])) = chequearAsignacion' x Map.empty
chequearAsignacion (Secuencia (x:xs)) = Map.union (chequearAsignacion' x Map.empty) (chequearAsignacion (Secuencia xs))

{-|
  Recorre un árbol de expresiones en búsqueda de asignaciones, cuando
  las encuentra, va almacenando en un mapa de símbolos auxiliar las
  variables involucradas en dichas funciones. Este mapa de símbolos
  resultante sera contrastado al final con el mapa de símbolos resultante
  del análisis sintáctico del resto del programa.
-}
chequearAsignacion' :: Expresion -- ^ Árbol de expresiones.
                    -> Map.Map String Symbol -- ^ Almacenamiento del mapa de símbolos a generar
                    -> Map.Map String Symbol -- ^ Mapa de símbolos resultante
chequearAsignacion' exp map = case exp of
                               Union x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Interseccion x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Diferencia x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Cartesiano x y -> Map.union (chequearAsignacion' x map) (chequearAsignacion' y map)
                               Partes x -> Map.union map (chequearAsignacion' x map)
                               Complemento x -> Map.union map (chequearAsignacion' x map)
                               OpExtension (ConjuntoExt set gens fils) -> Map.union map (chequearGenerador gens)
                               Asignacion var x -> Map.union (Map.insert (takeStr var) (Symbol (Nothing, Just (Conjunto (SetC.emptySet) (Dominio (SetC.emptySet))))) map) (chequearAsignacion' x map)
                               OpId var -> Map.insert (takeStr var) (Symbol (Nothing, Just (Conjunto (SetC.emptySet) (Dominio (SetC.emptySet))))) map
                               OpUniverso(UniversoDe var) -> Map.insert (takeStr var) (Symbol (Nothing, Just (Conjunto (SetC.emptySet) (Dominio (SetC.emptySet))))) map
                               _ -> Map.empty

{-|
  Crea un mapa de símbolos con las variables usadas en los
  lados derechos de cada generador de un conjunto por extensión,
  este mapa luego será constrastado contra el mapa de símbolos
  original que se ha parseado.
-}
chequearGenerador :: [Generador] -- ^ Lista de generadores  
                  -> Map.Map String Symbol -- ^ Mapa generado con las variables de los lados derechos de los generadores.
chequearGenerador [] = Map.empty
chequearGenerador ((Gen x y):xs) = Map.union (Map.singleton (takeStr y) (Symbol (Nothing, Just (Conjunto (SetC.emptySet) (Dominio (SetC.emptySet)))))) (chequearGenerador xs)

{-|
  Devuelve el dominio asociado a un /Symbol/
-}
takeDom :: Symbol -- ^ /Symbol/ sobre el que se quiere su dominio asociado.
        -> Dominio -- ^ Dominio encontrado en el /Symbol/.
takeDom (Symbol (Just a, _)) = a
takeDom (Symbol (Nothing, _)) = error $ "Error 0x08042FFA"

{-|
  Análogamente a la funcion anterior, devuelve el conjunto asociado
  a un /Symbol/.
-}
takeConj :: Symbol  -- ^ /Symbol/ sobre el que se quiere su conjunto asociado.
         -> Conjunto -- ^ Conjunto encontrado en el /Symbol/.
takeConj (Symbol (_, Just a)) = a
takeConj (Symbol (_, Nothing)) = error $ "Error 0x08042FFD"

{-|
  Función que devuelve el string envuelto por un Token cuyo constructor
  sea TkStr o TkId.
-}
takeStr :: Token -> String
takeStr (TkStr pos s) = s
takeStr (TkId pos s) = s

