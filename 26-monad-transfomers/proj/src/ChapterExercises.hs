import           Control.Monad.Trans.Reader

rDec :: Num a => Reader a a
rDec = reader (\x -> x - 1)
