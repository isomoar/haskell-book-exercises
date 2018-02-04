module Types.Validation where


import           Data.Semigroup  (Semigroup, (<>))
import           Test.QuickCheck (Arbitrary (..), arbitrary, frequency)


data Validation a b = Failure a | Success b deriving (Show, Eq)


newtype AccumulateRight a b =
  AccumulateRight (Validation a b) deriving (Eq, Show)

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b) deriving (Eq, Show)



instance Semigroup a => Semigroup (Validation a b) where
  (Success a) <> (Success b) = Success b
  (Success a) <> (Failure b) = Failure b
  (Failure a) <> (Failure b) = Failure (a <> b)
  (Failure a) <> (Success b) = Failure a


instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success a) <> AccumulateRight (Success b) =
    AccumulateRight (Success (a <> b))
  AccumulateRight (Failure a ) <> AccumulateRight (Success b) =
    AccumulateRight (Failure a)
  _ <> AccumulateRight (Failure b) = AccumulateRight (Failure b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ Failure a) , (1, return $ Success b)]

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (AccumulateRight a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return $ AccumulateRight (Failure a))
                , (1, return $ AccumulateRight (Success b)) ]


type ValidationAssoc = Validation String [Int]
                    -> Validation String [Int]
                    -> Validation String [Int]
                    -> Bool


type ValidationRightAssoc = AccumulateRight String [Int]
                          -> AccumulateRight String [Int]
                          -> AccumulateRight String [Int]
                          -> Bool
