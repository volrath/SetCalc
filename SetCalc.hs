{-|
   /Programa Principal - Calculadora/
   
   Traductores e Interpretadores CI3725
   

   05-38675 Kristoffer Pantic <sktdude@gmail.com>

   04-36723 Daniel Barreto N. <daniel@ac.labf.usb.ve>
   
   Programa Principal para el interpretador de la calculadora.

 -}
module Main (
-- * Función Principal.
  main
) where

import System.IO
import System.Exit
import System(getArgs)
import qualified Control.Exception as C
import qualified Data.Map as Map
import Lexer
import Parser
import Abstract

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
           let loop = do
                 hSetBuffering stdout NoBuffering
                 line <- promptAndGet
                 catchOrPrint (parser $ lexer line)
                 loop
           loop
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
promptAndGet =
    putStr "SetCalc> "
    >> getLine

{-|
  catchOrPrint

  Función que recibe la ejecucion del parser y el lexer sobre un
  string, trata de imprimir dicha instruccion pero si hubo errores
  los imprime por la salida de error estandar y permite recuperar
  el prompt.
-}
catchOrPrint ::  TupParser -- ^ La ejecución de las funciones parser y lexer sobre un string.
             -> IO() -- ^ La impresión del resultado de la ejecución de la función.
catchOrPrint tup = C.catch (print tup) fail
    where fail e = hPrint stderr e




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
