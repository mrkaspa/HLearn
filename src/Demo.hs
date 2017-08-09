module Demo
  ( State(On, Off)
  , New(..)
  , ListInt
  , sumi
  , sumix
  , sumiz
  , Optional(..)
  ) where

import Data.Monoid
import Test.QuickCheck (Arbitrary(..), frequency)

data State
  = On
  | Off
  deriving (Show, Eq)

data New = New
  { name :: String
  , age :: Int
  , size :: Int
  }

data User = User
  { name :: String
  , age :: Int
  , size :: Int
  }

instance Show New where
  show new =
    "User info: " ++ name (new :: New) ++ " --- " ++ show (age (new :: New))

type ListInt = [Int]

demo = New {name = "demo", age = 1, size = 2}

sumi a b
  | a < 0 = b
  | b < 0 = a
  | otherwise = a + b

sumix (Just a) (Just b) = a + b

sumiz a b = do
  a' <- a
  b' <- b
  return (a' + b')

fac :: (Eq a, Num a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

facT :: (Eq a, Num a) => a -> a
facT 0 = 1
facT n = facT' 1 n
  where
    facT' acc 0 = acc
    facT' acc n = facT' (acc * n) (n - 1)

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (mappend a b)
  mappend (Only a) Nada = Only a
  mappend Nada (Only a) = Only a
  mappend Nada Nada = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (1, fmap Only arbitrary)]

type Verb = String

type Adjective = String

type Adverb = String

type Noun = String

type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  mconcat
    [ e
    , "! he said "
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove off with this "
    , adj
    , " wife."
    ]
