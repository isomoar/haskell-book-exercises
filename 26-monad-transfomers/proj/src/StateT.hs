{-# LANGUAGE InstanceSigs #-}
import           Control.Applicative

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s ->
    let g (a, b) = (f a, b)
    in fmap g (sma s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \x -> pure (a, x)

  -- smab :: s -> m (a -> b, s)
  -- sma :: s -> m (a, s)
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
      (f, s') <- smab s
      (a, s'') <- sma s
      return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a
          -> (a -> StateT s m b)
          -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s







