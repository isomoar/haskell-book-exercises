{-# LANGUAGE InstanceSigs #-}


import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (\(a, s) -> (f a, s)) . g

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (a, s') = g s
          (fa, fs) = f s
      in (fa a,  s')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi $ \s ->
      let (a, s') = f s
          Moi g' = g a
          (ga, gs) = g' s
      in (ga, s')

