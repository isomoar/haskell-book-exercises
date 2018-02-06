lefts' :: [Either a b] -> [a]
lefts' = foldr foldFunc []
  where foldFunc (Left a) acc = a : acc
        foldFunc _ acc        = acc


rights' :: [Either a b] -> [b]
rights' = foldr foldFunc []
  where foldFunc (Right a) acc = a : acc
        foldFunc _ acc         = acc


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr foldFunc ([], [])
  where foldFunc (Right a) (l, r) = (l, a : r)
        foldFunc (Left a) (l, r)  = (a : l, r)


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a)  = Nothing
eitherMaybe' f (Right a) = Just (f a)


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a)  = f a
either' f g (Right a) = g a


eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (\x -> Just $ f x) x
