module Main where

import           Moi

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put x = Moi $ \_ -> ((), x)

exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa

eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ( (), f s )

main :: IO ()
main = do
  putStrLn "abc"

