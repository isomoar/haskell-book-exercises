newtype HumanName =
  HumanName String deriving (Eq, Show)

newtype DogName =
  DogName String deriving (Eq, Show)

newtype Address =
  Address String deriving (Eq, Show)

data Person =
  Person {
        humanName :: HumanName
      , dogName   :: DogName
      , address   :: Address
   } deriving (Eq, Show)

data Dog =
  Dog {
        dogsName    :: DogName
      , dogsAddress :: Address
      } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogRA2 :: Person -> Dog
getDogRA2 =
  liftA2 Dog dogName address

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy
