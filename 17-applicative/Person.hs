validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s


newtype Name = Name String deriving (Eq, Show)
newtype LastName = LastName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)


mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s


mkLastName :: String -> Maybe LastName
mkLastName s = fmap LastName $ validateLength 5 s


mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person =
  Person Name LastName Address
  deriving (Eq, Show)


mkPerson :: String -> String -> String -> Maybe Person
mkPerson n l a =
  -- without Applicative:
  --
  -- case mkName n of
  --   Nothing -> Nothing
  --   Just n' ->
  --     case mkAddress a of
  --       Nothing -> Nothing
  --       Just a' -> Just $ Person n' a'
  --
  -- with Applicative:
  Person <$> mkName n <*> mkLastName l <*> mkAddress a
