{-|
   /Programa Principal - Calculadora/
   
   Traductores e Interpretadores CI3725
   

   05-38675 Kristoffer Pantic <sktdude@gmail.com>

   04-36723 Daniel Barreto N. <daniel@ac.labf.usb.ve>
   
   Programa Principal para el interpretador de la calculadora.

 -}
module Main (
-- * Función Principal.
  main,
  promptAndGet
) where

import System.IO
import System.Exit
import System(getArgs)
import qualified Control.Exception as C
import Lexer

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
           putStr "Analizador lexicografico:\n"
           let loop = do
                 hSetBuffering stdout NoBuffering
                 line <- promptAndGet
                 printTokensOrCatch (lexer line)
                 loop
           loop
         else do
           -- Se abre el archivo y se analiza
           let iowork = map scanFile args
           sequence_ iowork

-- Funciones auxiliares
{-|
  promptAndGet

  Función que se encarga de mostrar un prompt en el interpretador
  de la calculadora SetCalc y luego de retornar la línea
  introducida por el usuario.
-}
promptAndGet :: IO String -- ^ Línea leída por el usuario
promptAndGet =
    putStr "SetCalc> "
    >> getLine

{-|
  scanFile
  
  Función que abre un archivo @fpath@ pasado por parámetro, lee
  su contenido. Si hay alguna excepción es reportada y si no
  se imprime los tokens leidos en ese archivo.
-}
scanFile :: FilePath -- ^ Archivo a abrir.
         -> IO () -- ^ Resultado de la lectura del archivo.
scanFile fpath = do
  content <- readFileOrCatch fpath
  case content of
    Right tokenslist -> do
      printTokensOrCatch tokenslist
    Left e -> do
      hPutStr stderr ("Imposible abrir el archivo " ++ fpath ++ " debido a: ")
      hPrint stderr e

{-|
  readFileOrCatch
  
  Función que lee un archivo @fpath@ y devuelve la lista de tokens
  encontrados en ese archivo o una excepción de no haber
  podido abrir el archivo.
-}
readFileOrCatch :: FilePath -- ^ Archivo a abrir.
                -> IO (Either C.IOException [Token]) -- ^ Si no hubo fallos, devuelve una lista de tokens, en otro caso devuelve una Excepción
readFileOrCatch fpath = catch (try fpath) fail
    where
      try f = do
        c <- readFile f
        let tokens = lexer c
        return $ Right tokens
      fail e = return (Left e)

{-|
  printTokensOrCatch
  
  Función que recibe una lista de @tokens@, trata de imprimirlos
  y si encuentra una excepción, lo imprime por la la salida
  estándar de errores.
-}
printTokensOrCatch :: [Token] -- ^ Lista de tokens a imprimir
                   -> IO () -- ^ Impresión del resultado
printTokensOrCatch tokens = C.catch (printOrKill tokens) fail
    where fail e = do
            if (e == C.ExitException ExitSuccess)
               then C.throwIO e
               else hPrint stderr e

{-|
  printOrKill
  
  Función que verifica si el /token/ @TkFin@ es el primer
  token de la lista de /tokens/ pasada por parámetro, en
  este caso devuelve una excepción de salida. De lo
  contrario imprime la lista de /tokens/
-}
printOrKill :: [Token] -- ^ Lista de tokens
            -> IO () -- ^ Impresión de la lista de tokens
printOrKill tokens = do
  if (head tokens) == TkFin (1,1)
     then exitWith ExitSuccess
     else print tokens