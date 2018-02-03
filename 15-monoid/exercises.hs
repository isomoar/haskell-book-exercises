import qualified Data.Monoid     (Monoid, Sum, mappend)
import           Data.Semigroup  as S (Semigroup, (<>))
import           Test.QuickCheck


-- Datatype definitions


data Trivial = Trivial deriving (Eq, Show)

newtype Identity a = Identity a deriving (Eq, Show)

data Two a b = Two a b deriving (Eq, Show)

data Three a b c = Three a b c deriving (Eq, Show)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

data Or a b = Fst a | Snd b deriving (Eq, Show)


-- Semigroup instances


instance Semigroup Trivial where
  _ <> _ = Trivial

instance (Monoid a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (mappend x y)

instance (Monoid a, Monoid b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = (Two (x1 `mappend` x2) (y1 `mappend` y2))

instance (Monoid a, Monoid b, Monoid c)
    => Semigroup (Three a b c) where
  (Three a1 b1 c1) <> (Three a2 b2 c2) =
    (Three (a1 `mappend` a2)
           (b1 `mappend` b2)
           (c1 `mappend` c2)
    )

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> (Fst b) = Snd a
  (Snd a) <> (Snd b) = Snd b


-- Arbitrary instances
--

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a<- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return $ BoolConj True)
                        , (1, return $ BoolConj False)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return $ Fst a), (1, return $ Snd b)]


-- Tests

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity String
                    -> Identity String
                    -> Identity String
                    -> Bool

type TwoAssoc = Two String (M.Sum Int)
                  -> Two String (M.Sum Int)
                  -> Two String (M.Sum Int)
                  -> Bool

type ThreeAssoc = Three String (M.Sum Int) [Int]
                  -> Three String (M.Sum Int) [Int]
                  -> Three String (M.Sum Int) [Int]
                  -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
