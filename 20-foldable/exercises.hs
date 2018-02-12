import           Data.Monoid

data Constant a b = Constant a deriving ( Show)

data Two a b = Two a b deriving (Show)

data Three a b c = Three a b c

data Three' a b = Three' a b b

data Four' a b = Four' a b b b

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Foldable (Two a) where
  foldMap f (Two a b) = f b
  foldr f z (Two a b) = f b z

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
  foldr f z (Three a b c) = f c z

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d


filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then (pure x) else mempty)


