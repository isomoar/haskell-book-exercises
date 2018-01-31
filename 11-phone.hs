module DaPhone where

import           Data.Char
import           Data.List
import           Data.Maybe

type Digit = Char
type Presses = Int
type Symbols = String

-- Phone layout:
-----------------------------------------
-- |  1     |  2ABC  |  3DEF  |
-- _____________________________________
-- |  4GHI  |  5JKL  |  6MNO  |
-----------------------------------------
-- |  7PQRS |  8TUV  |  9WXYZ |
-----------------------------------------
-- |  *^    |  0+_   |  #.,   |
-----------------------------------------

data Phone = Phone [(Digit, Symbols)] deriving (Eq, Show)

phone = Phone
    [ ('1', "1")
    , ('2', "abc")
    , ('3', "def")
    , ('4', "ghi")
    , ('5', "jkl")
    , ('6', "mno")
    , ('7', "pqrs")
    , ('8', "tuv")
    , ('9', "wxyc")
    , ('*', "*^")
    , ('0', "+_")
    , ('#', "#.,") ]

convo :: [String]
convo =
   ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]


-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone list) ch = case find (\x -> ch `elem` (snd x)) list of
                                Nothing     ->
                                  case isUpper ch of
                                    True -> ('*', 1) : reverseTaps phone (toLower ch)
                                    False -> []
                                Just (a, b) ->
                                  let presses = (+1) $ fromMaybe 0 $ (elemIndex ch b)
                                  in [(a, presses)]


cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

-- How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\a b -> b + (snd a)) 0



