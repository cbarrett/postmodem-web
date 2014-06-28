{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}

module Web.Postmodem.App 
  ( appMain
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Lazy hiding (index)
import Data.Time.Clock
import Data.Monoid
import Network.Wai.Middleware.Static
import Text.Blaze.Html.Renderer.Text
import Web.Scotty.Trans

import Web.Postmodem.Cache
import Web.Postmodem.Feed
import Web.Postmodem.Pages

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
    
    middleware . staticPolicy $ addBase "public"
    
    get "/episode/:idx" $ do
      eps <- getEps
      idx <- param "idx"
      let numEps = Prelude.length eps
      html $ renderHtml $ episode idx (eps !! (numEps - idx))
    
    get "/" $ do
      eps <- getEps
      html $ renderHtml $ index eps
    
