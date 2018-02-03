import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a


monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


-- data Bull = Fools | Twoo deriving (Eq, Show)
--
-- instance Arbitrary Bull where
--   arbitrary = frequency [ (1, return Fools)
--                         , (1, return Twoo)]
--
-- instance Monoid Bull where
--   mempty = Fools
--   mappend _ _ = Fools
--
-- type BullMappend = Bull -> Bull -> Bull -> Bool

-- testInvalidMonoid :: IO ()
-- testInvalidMonoid = do
--   quickCheck (monoidAssoc :: BullMappend)
--   quickCheck (monoidLeftIdentity :: Bull -> Bool)
--   quickCheck (monoidRightIdentity :: Bull -> Bool)

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x            = x
  mappend x Nada            = x
  mappend (Only x) (Only y) = Only (mappend x y)


newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance  Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) x             = x
  mappend x _                         = x

optionalGen :: Arbitrary a => Gen (Optional a)
optionalGen = do
  a <- arbitrary
  frequency [ (1, return Nada )
            , (1, return (Only a))
            ]

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = optionalGen

firstGen :: Arbitrary a => Gen (First' a)
firstGen = do
  a <- arbitrary
  frequency [ (1, return (First' Nada) )
            , (1, return (First' a))
            ]

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = firstGen

type FirstAssoc = First' Int -> First' Int -> First' Int -> Bool

testFirst :: IO ()
testFirst = do
  quickCheck (monoidAssoc :: FirstAssoc)
  quickCheck (monoidLeftIdentity :: First' Int -> Bool)
  quickCheck (monoidRightIdentity :: First' Int -> Bool)
