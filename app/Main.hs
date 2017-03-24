module Main where

import           Lib

main :: IO ()
main = do
  putStrLn "Ingresa tu nombre: "
  name <- getLine
  printNewName name
