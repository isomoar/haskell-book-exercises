import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a          = a
  mappend a Nil          = a
  mappend (Cons a as) bs = Cons a (as <> bs)

instance Functor List where
  fmap f Nil           = Nil
  fmap f (Cons a list) = Cons (f a) (fmap f list)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil                  = Nil
  Nil <*> _                  = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

take' :: Int -> List a -> List a
take' _ Nil        = Nil
take' 1 (Cons x _) = Cons x Nil
take' n (Cons x l) = Cons x (take' (n - 1) l)


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> (List a)
repeat' x = xs
  where xs = Cons x xs

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f Nil _                   = Nil
zipWith' f _ Nil                   = Nil
zipWith' f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith' f as bs)


instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  _ <*> (ZipList' Nil)            = ZipList' Nil
  (ZipList' Nil) <*> _            = ZipList' Nil
  (ZipList' xs) <*> (ZipList' ys) = ZipList' (zipWith' ($) xs ys)


instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary


main :: IO ()
main = do
  quickBatch $ applicative (ZipList' (Cons ("a", "b", 1 :: Int) Nil))
