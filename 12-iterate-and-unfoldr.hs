myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)


myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Just (a, b) -> a : myUnfoldr f b
                  Nothing     -> []

beterIterate :: (a -> a) -> a -> [a]
beterIterate f x = myUnfoldr (\a -> Just (a, f a)) x
