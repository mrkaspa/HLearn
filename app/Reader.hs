module Main where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

main :: IO ()
main = mainWithText

mainWithText :: IO ()
mainWithText = do
  [filepath] <- getArgs
  text <- TIO.readFile filepath
  let wordsList = procText text
  TIO.putStrLn $ T.unwords wordsList

mainWithString :: IO ()
mainWithString = do
  [filepath] <- getArgs
  text <- readFile filepath
  let wordsList = procString text
  putStrLn $ concat $ intersperse " " wordsList

procText :: T.Text -> [T.Text]
procText text =
  map head $
  group $
  sort $
  map T.toCaseFold $
  filter (not . T.null) $ map (T.dropAround $ not . isLetter) $ T.words text

procString :: String -> [String]
procString text = map head $ group $ map stringToLower $ takeCleanWords $ text

stringToLower :: String -> String
stringToLower = map toLower

takeCleanWords :: String -> [String]
takeCleanWords = cleanWords . words

cleanWords :: [String] -> [String]
cleanWords = map (takeWhile isLetter . dropWhile (not . isLetter))
