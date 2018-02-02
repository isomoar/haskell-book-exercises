module WordNumberTest where

import           Data.List       (sort)
import           Test.Hspec
import           Test.QuickCheck
import           WordNumber      (digitToWord, digits, wordNumber)


-- test properties
--
half x = x / 2

halfIdentity = (*2) . half

prop_lessThanAbs :: Double -> Bool
prop_lessThanAbs x = (half x) > (abs x)

prop_next :: Double -> Bool
prop_next x = (halfIdentity x) == x

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)

plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative  = plusAssociative

plusCommutative x y =
  x + y == y + x

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative  = plusCommutative

prop_listOrdered :: [Int] -> Bool
prop_listOrdered list = if listOrdered list then True else False

testProperties :: IO ()
testProperties = hspec $ do
  it "x / 2 is always less than abs x" $ do
    quickCheck prop_lessThanAbs
  it "(*2) . half is always == x" $ do
    quickCheck prop_next
  it "should be ordered list" $ do
    quickCheck prop_listOrdered
  it "x + (y + z) == (x + y) + z" $ do
    quickCheck prop_plusAssociative
  it "x + y == y + x" $ do
    quickCheck prop_plusCommutative

-- test WordNumber module

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"


  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]


  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

