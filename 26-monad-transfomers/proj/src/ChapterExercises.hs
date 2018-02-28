import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = reader (flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  v <- ask
  lift (putStrLn $ "Hi: " ++ show v)
  return (v + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  v <- get
  lift (putStrLn $ "Hi: " ++ show v)
  put (v + 1)
  return (show v)


