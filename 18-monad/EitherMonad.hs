module EitherMonad where

-- years ago
type Founded = Int


-- number of programmers
type Coders = Int


data SoftwareShop =
  Shop { founded :: Founded, programmers :: Coders } deriving (Eq, Show)


data FoundedError =
  NegativeYears Founded
      | TooManyYears Founded
      | NegativeCoders Coders
      | TooManyCoders Coders
      | TooManyCodersForYears Founded Coders
      deriving (Eq, Show)


validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n


validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n


mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programers <- validateCoders coders
  if programers > div founded 10
     then Left $ TooManyCodersForYears founded programers
     else Right $ Shop founded programers


data Sum a b = First a | Second b deriving (Eq, Show)


instance Functor (Sum a) where
  fmap f (First x)  = First x
  fmap f (Second x) = Second (f x)


instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  _ <*> (First x) = First x
  Second x <*> Second y = Second (x y)


instance Monad (Sum a) where
  return = pure
  (First x) >>= _ = First x
  (Second x) >>= k = k x
