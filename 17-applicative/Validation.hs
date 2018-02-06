import           Control.Applicative
import           Test.QuickCheck          (Arbitrary (..), arbitrary, elements)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation err a =
        Failure err
      | Success a
      deriving (Eq, Show)


instance Functor (Validation e) where
  fmap f (Failure a) = Failure a
  fmap f (Success a) = Success (f a)


instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure x) <*> (Failure y) = Failure (mappend x y)
  _ <*> (Failure x) = Failure x
  (Failure x) <*> _  = Failure x
  (Success f) <*> (Success x) = Success (f x)


instance (Arbitrary err, Arbitrary a)
    => Arbitrary (Validation err a) where
  arbitrary = do
    err <- arbitrary
    a <- arbitrary
    elements [ Failure err, Success a]


data Errors =
        DividedByZero
      | StackOverflow
      | MogglesChewedWires
      deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a)   = Right a


eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a)  = Success a


main :: IO ()
main = do
  -- it complaints on Monoid constraint I can't figure out why so whatever
  quickBatch $ applicative (Success ("a", "b", 1 :: Int))
