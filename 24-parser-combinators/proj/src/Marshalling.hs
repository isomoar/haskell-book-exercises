{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshalling where

import           Control.Applicative
import           Data.Aeson
-- import           Data.Aeson.Utils     (floatingOrInteger)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Scientific      (floatingOrInteger)
import           Data.Text            (Text)
import           Text.RawString.QQ

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData =
  TestData {
  section    :: Host
      , what :: Color
           } deriving (Eq, Show)

newtype Host =
  Host String
  deriving (Eq, Show)

type Annotation = String

data Color =
  Red Annotation
      | Blue Annotation
      | Yellow Annotation
      deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ =
    fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) =
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
    (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"

data NumberOrString = Numba Integer | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of
      (Left _)        -> fail "Must be integral number"
      (Right integer) -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ =
    fail "NumberOrString must be number or string"

dec :: LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDec

main = do
  -- let d = decode sectionJson :: Maybe TestData
  print $ dec "\"blah\""
