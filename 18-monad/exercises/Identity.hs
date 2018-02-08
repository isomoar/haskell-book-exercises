import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


newtype Identity a = Identity a deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)


instance Monad Identity where
  return = pure
  (Identity a) >>= k = k a


instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary


instance (Eq a) => EqProp (Identity a) where (=-=) = eq


trigger = undefined :: Identity (String, String, Int)


main :: IO ()
main = do
  -- quickBatch $ applicative trigger
  quickBatch $ monad trigger


