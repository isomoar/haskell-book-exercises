module Types.Or where


import           Data.Semigroup  (Semigroup, (<>))
import           Test.QuickCheck

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> (Fst b) = Snd a
  (Snd a) <> (Snd b) = Snd b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return $ Fst a), (1, return $ Snd b)]

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool


