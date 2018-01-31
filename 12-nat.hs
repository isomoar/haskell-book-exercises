data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | otherwise = Just $ mkNat n
    where mkNat 0 = Zero
          mkNat i = Succ (mkNat (i-1))

