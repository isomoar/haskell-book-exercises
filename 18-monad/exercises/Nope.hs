import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show, Eq)


instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg


instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg


instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg


instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg


instance (Eq a) => EqProp (Nope a) where (=-=) = eq

trigger = undefined :: Nope (Int, String, Int)

main :: IO ()
main = do
  quickBatch $ monad trigger
