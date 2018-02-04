import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Types.Four
import           Types.Identity
import           Types.Pair
import           Types.Three
import           Types.Two

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f


functorCompose :: (Eq (f c), Functor f) =>
                    (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


-- functorCompose' :: (Eq (f c), Functor f) =>
--                       f a
--                     -> Fun a b
--                     -> Fun b c
--                     -> Bool
-- functorCompose' x (Fun _ f) (Fun _ g) =
--   (fmap (g . f) x) == (fmap g . fmap f $ x)
--

-- type Iden = Fun (Identity Int) (Identity Int)
-- type IdentityFC = (Identity Int) -> Iden -> Iden -> Bool


test :: IO ()
test = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Identity Int)
  quickCheck $ \x -> functorIdentity (x :: Pair Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Pair Int)
  quickCheck $ \x -> functorIdentity (x :: Two Int String)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Two Bool Int)
  quickCheck $ \x -> functorIdentity (x :: Three Bool String Int)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three Bool String Int)
  quickCheck $ \x -> functorIdentity (x :: Three' Int String)
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Three' Bool Int)
  quickCheck $ \x -> functorIdentity (x :: Four Int String Bool [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: Four Bool String Int Int)
  quickCheck $ \x -> functorIdentity (x :: Four' Int String)
