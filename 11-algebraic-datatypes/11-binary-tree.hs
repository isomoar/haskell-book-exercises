import           Data.Char

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq, Ord)

insertTree :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertTree a Leaf = Node Leaf a Leaf
insertTree a (Node left b right)
    | a == b = Node left b right
    | a < b = Node (insertTree a left) b right
    | a > b = Node left b (insertTree a right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

toList :: BinaryTree a -> [a]
toList Leaf                = []
toList (Node left a right) = toList left ++ [a] ++ toList right

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf                = acc
foldTree f acc (Node left a right) = foldTree f middle right
  where leftFold = foldTree f acc left
        middle = f a leftFold

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xs ys =
  foldr (\a b -> if not (a `elem` ys) then False else b ) True xs

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map mapFunc (words xs)
  where mapFunc w@(x:xs) = (w, toUpper x : xs)



