import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant { getConstant = x }

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant $ x <> y

instance Foldable (Constant a)  where
  foldMap _ _ = mempty

instance Monoid a => Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

type IIS = ([Sum Int], String, String)

main :: IO ()
main = do
  let trigger = undefined :: Constant IIS IIS
  quickBatch $ traversable trigger
