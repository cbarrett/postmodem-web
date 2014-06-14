{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Text.Lazy
import Data.Monoid
import Web.Scotty

main = scotty 3000 $ do
  counterT <- liftIO $ newTVarIO 0
  get "/:word" $ do
    beam <- param "word"
    times <- liftIO $ atomically $ do
      current <- readTVar counterT
      swapTVar counterT (current + 1)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1><h2>", pack (show times), "</h2>"]
