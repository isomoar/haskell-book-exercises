import           Control.Applicative
import           Test.QuickCheck
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
  (Failure x) <*> (Success _)  = Failure x
  (Success _) <*> (Failure x) = Failure x
  (Success f) <*> (Success x) = (Success (f x))


data Errors =
        DividedByZero
      | StackOverflow
      | MogglesChewedWires
      deriving (Eq, Show)


validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a)   = Right a


eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a)  = Success a


