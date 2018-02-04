module Types.Combine where

import           Data.Semigroup  (Semigroup, Sum (Sum, getSum), (<>))
import           Test.QuickCheck


newtype Combine a b =
  Combine { unCombine :: (a -> b) }


instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)


f :: Combine Integer (Sum Integer)
f = Combine $ \n -> Sum (n + 1)


g :: Combine Integer (Sum Integer)
g = Combine $ \n -> Sum (n - 1)


main :: IO ()
main =
    do  print $ unCombine (f <> g) $ 0
        print $ unCombine (f <> g) $ 1
        print $ unCombine (f <> f) $ 1
        print $ unCombine (g <> f) $ 1
