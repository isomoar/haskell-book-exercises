import           Data.Char
import           Data.List


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a


replaceThe :: String -> String
replaceThe = concat . intersperse " " . map mapFunc . words
  where mapFunc w = case notThe w of
                      Nothing -> "a"
                      Just a  -> a


isVowel :: Char -> Bool
isVowel x =
  let vowels = "aeiou"
  in x `elem` vowels || x `elem` (map toUpper vowels)


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . words
  where
    go n [] = n
    go n (x:xs) = case x of
                        "the" -> case xs of
                                   []  -> n
                                   ((nx:_):_) -> if isVowel nx
                                             then go (n+1) xs
                                             else go n xs
                        _     -> go n xs


countVowels :: String -> Integer
countVowels str = go 0 str
  where go n "" = n
        go n (x:xs) = case isVowel x of
                        True  -> go (n+1) xs
                        False -> go n xs
