module Demo
    ( State(On, Off),
      New(..),
      ListInt,
      sumi,
      sumix,
      sumiz
    ) where

data State = On | Off deriving (Show, Eq)
data New = New {name :: String, age  :: Int, size :: Int}

type ListInt = [Int]

demo = New {name="demo", age=1, size=2}

sumi a b
  | a < 0 = b
  | b < 0 = a
  | otherwise = a + b


sumix (Just a) (Just b) =
  a + b

sumiz a b = do
  a' <- a
  b' <- b
  a + b
