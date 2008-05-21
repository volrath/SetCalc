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
                 putStr "> "  -- Prompt
                 line <- getLine
                 print $ lexer line
                 loop
           loop
         else do
           -- Se abre el archivo y se analiza
           contents <- readFileOrCatch $ head args
           print $ lexer contents

{-|
  readFileOrCatch
-}
readFileOrCatch :: FilePath -> IO String
readFileOrCatch fpath = catch (readFile fpath) 
                        (\e -> 
                             hPutStr stderr ("Imposible abrir el archivo " ++ fpath ++ "\n")
                             >> exitFailure)
