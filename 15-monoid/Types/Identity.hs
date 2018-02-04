module Types.Identity where


import           Data.Semigroup  (Semigroup, (<>))
import           Test.QuickCheck

newtype Identity a = Identity a deriving (Eq, Show)

instance (Monoid a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (mappend x y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String
                    -> Identity String
                    -> Identity String
                    -> Bool
