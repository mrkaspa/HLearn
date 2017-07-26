module Main where

import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Intro your name: "
  name <- getLine
  putStrLn ("Your name " ++ name)
