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

instance Show New where
  show new = "User info: " ++ (name new) ++ " --- " ++ (show . age $ new)

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

fac :: (Eq a, Num a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

facT :: (Eq a, Num a) => a -> a
facT 0 = 1
facT n = facT' 1 n
  where
    facT' acc 0 = acc
    facT' acc n = facT' (acc * n) (n - 1)
