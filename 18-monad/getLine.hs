import           Control.Applicative ((*>))
import           Control.Monad       (join)


sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"


sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"


sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"


binding :: IO ()
binding = do
  name <- getLine
  putStrLn name


binding' :: IO ()
binding' =
  getLine >>= putStrLn


----------------------------------------------


bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
    \name -> putStrLn ("y helo thar: " ++ name)


twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ "yr old.")


twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn "age pls: " >>
  getLine >>=
  \age ->
  putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ "yr old.")


twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
  then [x*x, x*x]
  else [x]

----------------------

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n


g :: Integer -> Maybe Integer
g i =
  if even i
  then Just (i + 1)
  else Nothing


h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)


doSomething = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)


doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)
