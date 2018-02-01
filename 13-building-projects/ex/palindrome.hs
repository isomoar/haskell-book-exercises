import           Control.Monad
import           Data.Char     (isPunctuation, toLower)
import           System.Exit   (exitSuccess)

modify :: String -> String
modify = filter (not . isPunctuation) . map toLower . concat . words

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (modify line1 == (reverse $ modify line1) ) of
    True  -> putStrLn "It's a palindrome"
    False -> do
      putStrLn "Nope!"
      exitSuccess

