import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)


instance Functor List where
  fmap f Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Monoid (List a) where
  mempty = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys


instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f xs <*> ys = (f <$> ys) <> (xs <*> ys)


instance Monad List where
  return = pure
  Nil >>= k = Nil
  (Cons x xs) >>= k = (k x) <> (xs >>= k)


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = Cons <$> arbitrary <*> arbitrary


take' :: Int -> List a -> List a
take' n Nil         = Nil
take' 1 (Cons x _)  = Cons x Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs


instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys


-- functions


j :: Monad m => m (m a) -> m a
j m = m >>= id


l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = do
  x <- a
  return (f x)


l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = do
  x <- a
  y <- b
  return (f x y)


a :: Monad m => m a -> m (a -> b) -> m b
a mx mf  = do
  f <- mf
  x <- mx
  return (f x)

a' :: Monad m => m a -> m (a -> b) -> m b
a' mx mf = mf >>= (\f -> (mx >>= \x -> return (f x)))


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = sequence $ map f xs


flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms $ join . pure


main :: IO ()
main = do
  let trigger = undefined :: List (Int, String, Int)
  quickBatch $ monad trigger

