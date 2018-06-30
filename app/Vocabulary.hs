module Main where

import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

type Entry = (T.Text, Int) -- one entry

type Vocabulary = [Entry] -- a list of entries

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> processTextFile fname
    _ -> putStrLn "Usage: vocab-builder filename"

extractVocab :: T.Text -> Vocabulary
extractVocab text = map buildEntry $ group $ sort ws
  where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words text
    buildEntry ws = (w, length ws)
      where
        w = head ws
    cleanWord = T.dropAround (not . isLetter)

printAllWords :: Vocabulary -> T.Text
printAllWords vocab = do
  T.append "All words: \n" $ T.unlines $ map vocabToString vocab

vocabToString :: Entry -> T.Text
vocabToString (txt, count) = T.concat [txt, " = ", T.pack $ show count]

processTextFile :: FilePath -> IO ()
processTextFile fname = do
  text <- TIO.readFile fname
  let vocab = extractVocab text
  TIO.putStrLn $ printAllWords vocab
