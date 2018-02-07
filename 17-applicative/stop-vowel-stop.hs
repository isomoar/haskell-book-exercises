import           Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

one = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos s v s'= [(,,)] <*> s <*> v <*> s'
