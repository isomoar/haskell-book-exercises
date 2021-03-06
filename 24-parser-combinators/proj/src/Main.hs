{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.RawString.QQ
import           Text.Trifecta

type FractionOrNumber = Either Rational Integer

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseNof :: Parser FractionOrNumber
parseNof =
  try (Left <$> parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = do
  print $ parseString parseNof mempty "12"
