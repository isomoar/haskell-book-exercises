import           Data.Char

newtype Word' = Word' String  deriving (Eq, Show)

isVowel :: Char -> Bool
isVowel x =
  let vowels = "aeiou"
  in x `elem` vowels || x `elem` (map toUpper vowels)

mkWord :: String -> Maybe Word'
mkWord w = case vowels w > consonants w of
             False -> Just $ Word' w
             True  -> Nothing
 where
  vowels = length . filter ((==) True) . map isVowel
  consonants = length . filter ((/=) True) . map isVowel
