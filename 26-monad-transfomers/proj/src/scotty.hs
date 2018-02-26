{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy hiding (get)
import           Data.Monoid                    (mconcat)
import           Web.Scotty
import           Web.Scotty.Internal.Types      (ActionT (..))

-- newtype ActionT e m a =
--   ActionT {
--     runAM :: ExceptT (ActionError e)
--                      (ReaderT ActionEnv
--                           (StateT ScottyResponse m)) a }
--       deriving (Functor, Applicative)

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    -- (lift :: IO a -> ActionM a) (putStrLn "hello")
    (ActionT
      . (ExceptT . liftM Right)
      . (ReaderT . const)
      . \m -> StateT (\s -> do
                         a <- m
                         return (a, s))
      ) (putStrLn "hello")
    -- lift $ putStrLn "hello"
    html $ mconcat ["<h1>Scotty, ", beam, " me up! </h1>"]
