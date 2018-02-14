module ReaderPractice where

import           Control.Applicative
import           Data.Maybe
import           Data.Maybe
import           Data.Monoid

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

-- tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

uncurry' :: (a -> b -> c) -> (a , b) -> c
uncurry' f (a, b) = f a b

summed :: Num c => (c, c) -> c
summed = uncurry' (+)

bolt :: Integer -> Bool
bolt n = n > 3 && n < 8

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ getAll $ foldMap All $ sequA 6
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 9 ys

