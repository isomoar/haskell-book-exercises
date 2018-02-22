{-# LANGUAGE InstanceSigs #-}
import           Control.Applicative

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

-- instance (Functor m, Applicative m) => Functor (StateT s m) where
--   fmap f (StateT sma) = StateT $ \s ->
--     let ma = fmap (f . fst) (sma s)
--         ms = fmap snd (sma s)
--     in liftA2 (,) ma ms


instance (Functor m, Applicative m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    let ma = fmap (f . fst) (sma s)
        ms = fmap snd (sma s)
    in liftA2 (,) ma ms


