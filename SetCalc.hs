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
                 print $ lexer line
                 loop
           loop
         else do
           -- Se abre el archivo y se analiza
           let iowork = map scanFile args
           sequence_ iowork

promptAndGet :: IO String
promptAndGet =
    putStr "> "
    >> getLine

scanFile fpath = do
  content <- readFileOrCatch fpath
  case content of
    Right tokenslist -> putStrLn $ show tokenslist
    Left e -> do
      hPutStr stderr ("Imposible abrir el archivo " ++ fpath ++ " debido a: ")
      hPrint stderr e

{-|
  readFileOrCatch
-}
readFileOrCatch fpath = catch (try fpath) fail
    where
      try f = do
        c <- readFile f
        let tokens = lexer c
        return $ Right tokens
      fail e = return (Left e)
