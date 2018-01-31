myIterate :: (a -> a) -> a -> [a]
myIterate f n = n : myIterate f (f n)


-- myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- myUnfoldr f n = n : (myUnfoldr n)
