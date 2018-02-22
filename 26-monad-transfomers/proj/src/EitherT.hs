{-# LANGUAGE InstanceSigs #-}

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ema) = EitherT $ (fmap . fmap) f ema

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))

  (EitherT fab) <*> (EitherT mma) =
    EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  (EitherT mea) >>= f =
    EitherT $ do
      -- mea :: m (Either e a)
      -- v :: Either e a
      v <- mea
      case v of
        Left e  -> return (Left e)
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Left x)  = Right x
swapEither (Right x) = Left x

swapEitherT :: Monad m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ do
  v <- ema
  return (swapEither v)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT amb) = do
  v <- amb
  case v of
    Left x  -> f x
    Right x -> g x



