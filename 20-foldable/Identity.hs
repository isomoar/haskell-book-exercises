import           Data.Foldable
import           Data.Monoid


data Identity a = Identity a deriving (Eq, Show)

data Optional a = Nada | Yep a deriving (Eq, Show)


instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x


instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a


sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0


product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1


elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' e = foldr (\x acc -> if x == e then True else acc) False


null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True


length' :: (Foldable t) => t a -> Int
length' = foldr (\x acc -> acc + 1) 0


toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []
