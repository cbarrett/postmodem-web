{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Web.Postmodem.App 
  ( appMain
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Lazy
import Data.Time.Clock
import Data.Monoid
import Web.Scotty.Trans

import Web.Postmodem.Cache
import Web.Postmodem.Feed

data AppState = AppState {
  episodes :: TCache [Episode]
}
type AppActionM a = ActionT Text (ReaderT AppState IO) a

getEps :: AppActionM [Episode]
getEps = do
  now <- liftIO getCurrentTime
  epsT <- lift . asks $ episodes
  liftIO . atomically $ readTCache epsT now

appMain :: IO ()
appMain = do
  episodes <- newTCacheIO (fromIntegral 500) getEpisodes
  let runAppT r = runReaderT r AppState {episodes}
  scottyT 3000 runAppT runAppT $ do
    get "/" $ do
      eps <- getEps
      html . mconcat . fmap fmtEp $ eps

fmtEp :: Episode -> Text
fmtEp ep = mconcat ["<h1>", pack . title $ ep, "</h1>"]