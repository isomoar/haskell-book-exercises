module Addition where

import           Test.Hspec
import           Test.QuickCheck


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]


genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)


genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x - 1 > x


runQc :: IO ()
runQc = quickCheck prop_additionGreater


main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      ((1 + 1) :: Integer) > 1 `shouldBe` True
    it "22 divided by 5 is 4 remainder 2" $ do
      (dividedBy 22 5) `shouldBe` (4, 2)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
