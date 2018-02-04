{-# LANGUAGE FlexibleInstances #-}

data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap f (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


data More a b = L b a b | R a b a deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance   = Finance
  fmap f (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)


-- instance Functor (K a) where
--   fmap f (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

data EvilGoateeConst a b = GoatyConst b


data LiftItOut f a = LiftItOut (f a) deriving (Show, Eq)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut v) = LiftItOut (fmap f v)


data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa f' g') = DaWrappa (fmap f f') (fmap f g')

data IgnoreOne f g a b = IgnoreSomething (f a) (f b)

instance (Functor f) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething fa fb) = IgnoreSomething fa (fmap f fb)


data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)


data GoatLord a =
        NoGoat
            | OneGoat a
            | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
            deriving (Eq, Show)


instance Functor GoatLord where
  fmap f NoGoat            = NoGoat
  fmap f (OneGoat a)       = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)


data TalkToMe a =
  Halt
      | Print String a
      | Read (String -> a)
