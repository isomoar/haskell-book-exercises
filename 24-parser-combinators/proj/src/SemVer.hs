{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import           Control.Applicative
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import           Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord NumberOrString where
  (NOSS _) `compare` (NOSI _) = GT
  (NOSI _) `compare` (NOSS _) = LT
  (NOSS str) `compare` (NOSS str') = str `compare` str'
  (NOSI num) `compare` (NOSI num') = num `compare` num'

instance Ord SemVer where
  compare = compareSemVers

compareSemVers  :: SemVer -> SemVer -> Ordering
compareSemVers (SemVer mj mn p r _) (SemVer mj' mn' p' r' _) =
  mconcat comps
    where
      compVersions = zipWith compare [mj, mn, p] [mj', mn', p']
      compReleases = zipWith compare r r'
      compLen
          | length r > length r' = LT
          | length r < length r' = GT
          | otherwise = EQ
      comps = concat [compVersions, compReleases, [compLen]]

parseNos :: Parser NumberOrString
parseNos = do
  skipOptional (char '.')
  v <- (NOSI <$> natural) <|> (NOSS <$> some letter)
  return v

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  release <- option [] (char '-' >> some parseNos)
  meta <- option [] (char '+' >> some parseNos)
  return (SemVer major minor patch release meta)

main :: IO ()
main = do
  print $ parseString parseSemVer mempty "3.1.1-x.2.4.231+123"
