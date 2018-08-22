import qualified Lib as L
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "elem" $ do
    it "will find elements in a structure" $ do
      L.elem 3 [1, 2, 3, 4, 5] `shouldBe` True
    it "will fail if the element does not exist" $ do
      L.elem 8 [1, 2, 3, 4, 5] `shouldBe` False
  describe "minimimum" $ do
    it "will return Nothing on an empty List" $ do
      L.minimum ([] :: [Int])`shouldBe` (Nothing :: Maybe Int)
    it "will return the smallest value" $ do
      L.minimum ([25,24..20] :: [Int]) `shouldBe` Just 20
