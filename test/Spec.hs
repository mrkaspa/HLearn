import Demo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "sumiz" $ do
      it "should be Nothing" $ sumiz Nothing (Just 3) `shouldBe` Nothing
      it "should be 5" $ do (sumiz (Just 3) (Just 2)) `shouldBe` Just 5
    describe "props" $ do
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)
