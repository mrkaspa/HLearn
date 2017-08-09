import Data.Monoid
import Demo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "sumiz" $ do
      it "should be Nothing" $ sumiz Nothing (Just 3) `shouldBe` Nothing
      it "should be 5" $ sumiz (Just 3) (Just 2) `shouldBe` Just 5
    describe "props" $ do
      it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)
      it "monoid assoc property for string" $
        property (monoidAssoc :: String -> String -> String -> Bool)
      it "monoid assoc property for sum" $
        property (monoidAssoc :: Sum Int -> Sum Int -> Sum Int -> Bool)
      it "monoid assoc property for optionals" $
        property
          (monoidAssoc :: Optional String -> Optional String -> Optional String -> Bool)
      it "monoid left id" $ property (monoidLeftIdentity :: String -> Bool)
      it "monoid right id" $ property (monoidRightIdentity :: String -> Bool)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a
