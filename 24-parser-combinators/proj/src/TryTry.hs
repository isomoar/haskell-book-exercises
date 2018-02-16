{-# LANGUAGE QuasiQuotes #-}

module TryTry where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Fractions
import           Text.RawString.QQ
import           Text.Trifecta

type FractionOrNumber = Either Rational Integer

parseNof :: Parser FractionOrNumber
parseNof =
  try (Left <$> parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = do
  print $ parseString parseNof mempty "12"
