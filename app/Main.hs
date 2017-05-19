module Main where

import Lib
import Text.Read
import Timex
import Web

main1 :: IO ()
main1 = do
  putStrLn "Ingresa cuantas horas: "
  line <- getLine
  let hoursMaybe = readMaybe line :: Maybe Int
  case hoursMaybe of
    Just hours ->
      putStrLn $
      (show hours) ++ " hours to secs " ++ show (parseSec $ Hours hours)
    Nothing -> putStrLn "Bad input"

main:: IO ()
main = wmain
