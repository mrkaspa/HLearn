module Main where

import Text.Read
import Timex

main :: IO ()
main = do
  putStrLn "Ingresa cuantas horas: "
  line <- getLine
  let hoursMaybe = readMaybe line :: Maybe Int
  case hoursMaybe of
    Just hours ->
      putStrLn $
      (show hours) ++ " hours to secs " ++ show (parseSec $ Hours hours)
    Nothing -> putStrLn "Bad input"
