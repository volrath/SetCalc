{-|
   /Programa Principal - Calculadora/
   
   Traductores e Interpretadores CI3725
   

   05-38675 Kristoffer Pantic <sktdude@gmail.com>

   04-36723 Daniel Barreto N. <daniel@ac.labf.usb.ve>
   
   Programa Principal para el interpretador de la calculadora.

 -}
module Main (
-- * Funci�n Principal.
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

{-|
   Funci�n principal.

   El programa puede ser compilado para su ejecuci�n directa,
   o bien cargado en el interpretador GHCi e invocado a trav�s
   de la funci�n @main@.
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

  Funci�n que se encarga de mostrar un prompt en el interpretador
  de la calculadora SetCalc y luego de retornar la l�nea
  introducida por el usuario.
-}
promptAndGet :: IO String -- ^ L�nea le�da por el usuario
promptAndGet =
    putStr "SetCalc> "
    >> getLine

{-|
  scanFile
  
  Funci�n que abre un archivo @fpath@ pasado por par�metro, lee
  su contenido. Si hay alguna excepci�n es reportada y si no
  se imprime los tokens leidos en ese archivo.
-}
scanFile :: FilePath -- ^ Archivo a abrir.
         -> IO () -- ^ Resultado de la lectura del archivo.
scanFile fpath = do
  content <- readFileOrCatch fpath
  case content of
    Right parserResult -> do
      print parserResult
    Left e -> do
      hPutStr stderr ("Imposible abrir el archivo " ++ fpath ++ " debido a: ")
      hPrint stderr e

{-|
  readFileOrCatch
  
  Funci�n que lee un archivo @fpath@ y devuelve la lista de tokens
  encontrados en ese archivo o una excepci�n de no haber
  podido abrir el archivo.
-}
readFileOrCatch :: FilePath -- ^ Archivo a abrir.
                -> IO (Either C.IOException (Map.Map Var Symbol, AST)) -- ^ Si no hubo fallos, devuelve una lista de tokens, en otro caso devuelve una Excepci�n
readFileOrCatch fpath = catch (try fpath) fail
    where
      try f = do
        c <- readFile f
        let parserResult = parser $ lexer c
        return $ Right parserResult
      fail e = return (Left e)

{-|
  printTokensOrCatch
  
  Funci�n que recibe una lista de @tokens@, trata de imprimirlos
  y si encuentra una excepci�n, lo imprime por la la salida
  est�ndar de errores.
-}
printTokensOrCatch :: [Token] -- ^ Lista de tokens a imprimir
                   -> IO () -- ^ Impresi�n del resultado
printTokensOrCatch tokens = C.catch (print tokens) fail
    where fail e = do
            if (e == C.ExitException ExitSuccess)
               then C.throwIO e
               else hPrint stderr e
--     else print tokens