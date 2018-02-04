{-# LANGUAGE RankNTypes #-}

module BadNat where

type Nat f g = forall a. f a -> g a
