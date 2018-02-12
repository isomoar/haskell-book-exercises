data Constant a b = Constant a

data Two a b = Two a b

data Three a b c = Three a b c

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z
