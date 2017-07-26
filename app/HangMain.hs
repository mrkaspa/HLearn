module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.IO
import System.Random (randomRIO)

type WordList = [String]

data Puzzle =
  Puzzle String
         [Maybe Char]
         String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

minWordLength = 3

maxWordLength = 10

allWords :: IO WordList
allWords = do
  dict <- readFile "./data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l > minWordLength && l < maxWordLength

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, lastIndex)
  return $ wl !! randomIndex
  where
    lastIndex = length wl - 1

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (fmap (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that char"
      return puzzle
    (True, _) -> do
      putStrLn "The char is in the word :)"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "The char is not in the word :("
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if length guessed > (length wordToGuess * 2)
    then do
      putStrLn "Loser!"
      putStrLn $ "the word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "Winner!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = do
  hSetBuffering stdout NoBuffering
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ -> putStrLn "Your guess must be a single char"

mainName :: IO ()
mainName = do
  hSetBuffering stdout NoBuffering
  putStr "Intro your name: "
  name <- getLine
  putStrLn ("Your name is: " ++ name)
