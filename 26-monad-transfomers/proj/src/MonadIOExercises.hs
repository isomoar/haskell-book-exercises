import           Control.Applicative       (liftA2)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
-- import           Control.Monad.Trans.Identity

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT (fmap (fmap f) mma)

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure

  MaybeT mmab <*> MaybeT mma =
    MaybeT $ liftA2 (<*>) mmab mma

instance Monad m => Monad (MaybeT m) where
  return = pure

  MaybeT mma >>= f =
    MaybeT $ mma >>= maybe (pure Nothing) (runMaybeT . f)

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
