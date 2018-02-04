module Types.Two where

import           Data.Semigroup  (Semigroup, (<>))
import           Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = (Two (x1 `mappend` x2) (y1 `mappend` y2))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a<- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String [Int]
                  -> Two String [Int]
                  -> Two String [Int]
                  -> Bool


