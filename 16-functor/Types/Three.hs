module Types.Three where

import           Test.QuickCheck

data Three a b c = Three a b c deriving (Show, Eq)


data Three' a b = Three' a b b deriving (Show, Eq)


instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)


instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)


instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

