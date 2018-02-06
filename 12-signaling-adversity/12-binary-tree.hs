data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)


unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
               Just (left, a, right) -> Node leftUnfold a rightUnfold
                 where leftUnfold = unfold f left
                       rightUnfold = unfold f right
               Nothing -> Leaf


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where f x
            | x == n = Nothing
            | otherwise = Just (x + 1, x, x + 1)
