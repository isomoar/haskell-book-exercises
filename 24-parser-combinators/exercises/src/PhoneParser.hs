{-# LANGUAGE OverloadedStrings #-}
module PhoneParser where

import           Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

dotted :: Parser PhoneNumber
dotted = do
  p1 <- integer
  char '-'
  p2 <- integer
  char '-'
  p3 <- integer
  return $
    PhoneNumber (fromIntegral p1) (fromIntegral p2) (fromIntegral p3)

plain :: Parser PhoneNumber
plain = undefined

parensP :: Parser PhoneNumber
parensP = do
  p1 <- parens (integer)
  whiteSpace
  p2 <- integer
  char '-'
  p3 <- integer
  return $
    PhoneNumber (fromIntegral p1) (fromIntegral p2) (fromIntegral p3)

parsePhone :: Parser PhoneNumber
parsePhone = undefined

trifP :: String -> IO ()
trifP s =
  print $ parseString dotted mempty s
