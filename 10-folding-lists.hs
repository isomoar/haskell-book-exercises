-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f acc [] = acc
-- foldl f acc (x:xs) = foldl f (f acc x) xs
--

import           Data.Time

data DatabaseItem = DbString String
                   | DbNumber Integer
                   | DbDate UTCTime
                   deriving (Eq, Ord, Show)


toDatabase :: [DatabaseItem]
toDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items = foldr foldFunc [] items
  where foldFunc x acc = case x of
                           (DbDate d) -> acc ++ [d]
                           _          -> acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = foldr foldFunc [] items
  where foldFunc x acc = case x of
                           (DbNumber x) -> acc ++ [x]
                           _            -> acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = maximum $ filterDbDate items

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb items = (fromIntegral $ sumDb items) / (fromIntegral $ length $ filterDbNumber items)

stops = "pbtdkg"
vowels = "aeiou"

nouns = ["cow", "bird", "gay"]
verbs = ["make", "eat", "sleep"]

nounVerbNoun :: [String] -> [String] -> [(String)]
nounVerbNoun nouns verbs =
  [(n ++ " " ++ v ++ " " ++ n1) | n <- nouns, v <- verbs, n1 <- nouns]


seekritFunc :: String -> Double
seekritFunc x =
  (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))


myAnd :: [Bool] -> Bool
myAnd = foldr (\a b -> if a == False then False else b) True

myOr :: [Bool] -> Bool
myOr = foldr (\a b -> if a == True then True else b) False

myElem :: Eq a => a -> [a] -> Bool
myElem el = foldr (\a b -> if a == el then True else b) False

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> a ++ b) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b -> if f a b == GT then a else b) x xs
