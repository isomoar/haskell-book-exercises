import           Control.Monad
import           System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ ", Age was: " ++ show age


gimmePerson :: IO String
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please enter your person's age:"
  age <- getLine
  putStrLn "Your person's name:"
  name <- getLine
  let person = mkPerson name (read age)
      result = case person of
                 Left err -> show err
                 Right p  -> "Yay! Successfully got a person: " ++ (show p)
  return result


