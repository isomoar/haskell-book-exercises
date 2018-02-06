{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Data.Int

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42

-- instance TooMany (Int, Int) where
--   tooMany (a, b) = (a + b) > 100

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (tm1, tm2) = tooMany tm1 && tooMany tm2
data OperatingSystem =
  GnuPlusLinux
      | OpenBSD
      | Mac
      | Windows
      deriving (Eq, Show)

data ProgrammingLanguage =
  Haskell
      | Agda
      | Idris
      | Purescript
      deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgrammingLanguage}
           deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSD
  , Mac
  , Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lan |
                              os <- allOperatingSystems,
                              lan <- allLanguages]

data Quantum = Yes | No | Both deriving (Eq, Show)

