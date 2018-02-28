{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

data Config =
  Config {
    counts  :: IORef (M.Map Text Integer)
   , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  let (v, m') = M.updateLookupWithKey (\k x -> Just (x + 1)) k m
  in (m',fromMaybe 1 v)

changeEnv :: M.Map Text Integer -> ReaderT Config IO
changeEnv m = do
  counter <- newIORef m
  Config _ prefix <- ask
  local (\c -> Config counter prefix) ()


app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    Config countsRef prefix <- lift ask
    m <- liftIO $ readIORef countsRef
    let key' = mappend (TL.unpack prefix) unprefixed
        newMap = fst $ bumpBoomp (TL.pack key') m
        newInteger = snd $ bumpBoomp (TL.pack key') m
    w <- runReaderT (changeEnv newMap)
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , TL.pack $ unprefixed
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app
