import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Types.Identity

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f


functorCompose :: (Eq (f c), Functor f) =>
                    (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


functorCompose' :: (Eq (f c), Functor f) =>
                      f a
                    -> Fun a b
                    -> Fun b c
                    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

li x = functorCompose (+1) (*2) (x :: [Int])

test :: IO ()
test = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck li
  quickCheck (functorCompose' :: IntFC)
