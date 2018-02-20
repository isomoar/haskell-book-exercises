{-# LANGUAGE OverloadedStrings #-}
module NumbersParser where

import           Control.Applicative
import           Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = do
  d <- some parseDigit
  return (read d)

readNegative :: Parser Integer
readNegative = do
  m <- char '-'
  d <- some parseDigit
  return ( read $ (m : d) )

readPositive :: Parser Integer
readPositive = do
  skipOptional (oneOf "+")
  d <- some parseDigit
  return (read d)

base10Integer' :: Parser Integer
base10Integer' = try readNegative <|> readPositive

trifP :: String -> IO ()
trifP =
  print . parseString base10Integer' mempty

main :: IO ()
main =
  print $ parseString (some parseDigit) mempty "123"
