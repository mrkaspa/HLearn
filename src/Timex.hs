module Timex (
  Secs(..)
  , Minutes(..)
  , Hours(..)
  , Timex(..)
  , parseSec
  , compareTimex
  ) where

newtype Secs = Secs Int deriving Show

newtype Minutes = Minutes Int deriving Show

newtype Hours = Hours Int deriving Show

parseSecInt :: ToSeconds a => a -> Int
parseSecInt a = s
  where
    Secs s = parseSec a

class ToSeconds a where
  parseSec :: a -> Secs

instance ToSeconds Secs where
  parseSec s = s

instance ToSeconds Minutes where
  parseSec (Minutes m) = Secs (m * 60)

instance ToSeconds Hours where
  parseSec (Hours m) = Secs (m * 60 * 60)

newtype ToSeconds a => Timex a = Timex a deriving Show

instance ToSeconds a => Eq (Timex a) where
  (==) (Timex a) (Timex b) =
    aSecs == bSecs
    where
      aSecs = parseSecInt a
      bSecs = parseSecInt b

class Comparable a b where
  cmp :: a -> b -> Bool
  diff :: a -> b -> Bool
  diff a = not . cmp a

instance (ToSeconds a, ToSeconds b) => Comparable (Timex a) (Timex b) where
  cmp (Timex a) (Timex b) =
    aSecs == bSecs
    where
      aSecs = parseSecInt a
      bSecs = parseSecInt b

compareTimex :: (ToSeconds a, ToSeconds b) => a -> b -> Bool
compareTimex a b = cmp (Timex a) (Timex b)

instance Eq Secs where
  (==) (Secs a) (Secs b) = a == b

instance Eq Minutes where
  (==) a b = parseSec a == parseSec b

instance Eq Hours where
  (==) a b = parseSec a == parseSec b
