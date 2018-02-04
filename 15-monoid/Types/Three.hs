module Types.Three where


import           Data.Semigroup  (Semigroup, (<>))
import           Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c)
    => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) =
    (Three (a1 `mappend` a2)
           (b1 `mappend` b2)
           (c1 `mappend` c2)
    )

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc = Three String [Int] [Int]
                  -> Three String [Int] [Int]
                  -> Three String [Int] [Int]
                  -> Bool
