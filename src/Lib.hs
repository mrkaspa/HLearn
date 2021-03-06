module Lib
  ( someFunc
  , printNewName
  ) where

import Demo (New(..))

-- import qualified Demo
someFunc :: IO ()
someFunc = putStrLn "someFunc"

printNewName :: String -> IO ()
printNewName uname =
  let new = New {name = uname, age = 1, size = 10}
    -- putStrLn $ (name new) ++ " --- " ++ (show . age $ new)
  in print new

sumi
  :: (Fractional a, Eq a)
  => a -> a -> a
sumi = (+)

divi
  :: (Fractional a, Eq a)
  => a -> a -> a
divi a b
  | b == 0 = 0
  | otherwise = a / b

divi2
  :: (Fractional a, Eq a)
  => a -> a -> Maybe a
divi2 a 0 = Nothing
divi2 a b = Just (a / b)

divi3 a b =
  if b /= 0
    then a / b
    else 0

sumiDivi a b = (sumi a) . (divi b)

parsi :: Int -> String
parsi a = show a

printi :: String -> IO ()
printi a = putStr a

parsiPrinti = printi . parsi

sumiMonad
  :: (Num a)
  => Maybe a -> Maybe a -> Maybe a
sumiMonad (Just a) (Just b) = Just (a + b)
sumiMonad _ _ = Nothing

sumiMonad1
  :: (Num a)
  => Maybe a -> Maybe a -> Maybe a
sumiMonad1 a b = do
  a' <- a
  b' <- b
  return (a' + b')

sumList
  :: Num t
  => [t] -> t
sumList [] = 0
sumList (h:t) = h + sumList (t)

sumList1
  :: Num t
  => [t] -> t -> t
sumList1 [] acc = acc
sumList1 (h:t) acc = sumList1 t (acc + h)

sumList2
  :: Num t
  => [t] -> t
sumList2 listi =
  let sumList2' [] acc = acc
      sumList2' (h:t) acc = sumList2' t (h + acc)
  in sumList2' listi 0

sumList3
  :: Num t
  => [t] -> t
sumList3 listi = sumList2' listi 0
  where
    sumList2' [] acc = acc
    sumList2' (h:t) acc = sumList2' t (h + acc)

inc :: (Integer -> Integer) -> [Integer] -> [Integer]
inc incFn ls = map incFn ls

incBy1 :: [Integer] -> [Integer]
incBy1 ls = inc ((+) 1) ls

incBy2 :: [Integer] -> [Integer]
incBy2 = inc ((+) 1)
