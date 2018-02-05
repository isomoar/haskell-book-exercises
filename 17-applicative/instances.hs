import           Control.Applicative


newtype Identity a = Identity a deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity a)  = Identity (f a)


instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)


newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)


instance Functor (Constant a) where
  fmap f (Constant a) = Constant { getConstant = a }


instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant f) (Constant a) = Constant $ mappend f a
