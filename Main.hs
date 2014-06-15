{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Lazy
import Data.Monoid
import Web.Scotty.Trans

data AppState = AppState {
  counterT :: TVar Int
}
type AppActionM a = ActionT Text (ReaderT AppState IO) a
askCounterT = lift $ asks counterT

main = do
  counterT <- newTVarIO 0
  let initialState = AppState { counterT = counterT }
      runAppT r = runReaderT r initialState
  scottyT 3000 runAppT runAppT $ do
    get "/:word" getWord

getWord :: AppActionM ()
getWord = do
  counterT <- askCounterT
  beam <- param "word"
  times <- liftIO $ atomically $ do
    current <- readTVar counterT
    swapTVar counterT (current + 1)
  html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1><h2>", pack (show times), "</h2>"]
  