module LearningParsers where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' <* eof

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

sp :: Parser String
sp = string "12"

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParseString :: Parser String -> IO ()
testParseString p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo;:"
  testParse oneTwo'
