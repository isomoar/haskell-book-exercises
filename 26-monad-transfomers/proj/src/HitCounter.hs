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
  let f x = if x == Nothing then (Just 0) else (fmap (+1) x)
      m' = M.alter f k m
      v' = M.lookup k m'
  in (m', fromMaybe 0 v')

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    Config countsRef prefix <- lift ask
    m <- liftIO $ readIORef countsRef
    let key' = mappend prefix unprefixed
        (newCounts, newInteger) = bumpBoomp key' m
    liftIO $ writeIORef countsRef newCounts
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR m = runReaderT m config
  scottyT 3000 runR app
