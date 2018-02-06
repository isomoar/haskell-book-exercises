import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a            = a
  mappend a Nil            = a
  mappend (Cons a list) xs = Cons a (mappend list xs)


instance Functor List where
  fmap f Nil           = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)


instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil                  = Nil
  Nil <*> _                  = Nil
  (Cons f fl) <*> list = (fmap f list) <> (fl <*> list)

take' :: Int -> List a -> List a
take' = undefined

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith'_ Nil _ = Nil
zipWith' _ _ Nil                   = Nil
zipWith' f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith' f as bs)


instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (ZipList' fs) <*> (ZipList' xs) = ZipList' (zipWith' (\f x -> f x) fs xs)


instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend


-- instance (Arbitrary a) => Arbitrary (List a) where
--   arbitrary = Cons <*> arbitrary <*> arbitrary
--
--
-- instance Arbitrary a => Arbitrary (ZipList' a) where
--   arbitrary = ZipList' <$> arbitrary
--
--
-- v = ZipList' (Cons "a" Nil)  :: ZipList' String
--
-- main :: IO ()
-- main = do
--   quickBatch $ monoid v
