{-# LANGUAGE OverloadedStrings #-}

module MonadTransInstances where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.Reader
-- import           Control.Monad.Trans.State.Lazy hiding (get)

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift a = StateT $ \s -> do
    a' <- a
    return (a', s)


