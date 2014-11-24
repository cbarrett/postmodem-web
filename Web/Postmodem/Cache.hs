{-# LANGUAGE NamedFieldPuns #-}

module Web.Postmodem.Cache
  ( TCache
  , newTCacheIO
  , readTCache
  ) where

import Control.Concurrent.STM
import Data.Time.Clock
import GHC.Conc

data TCache a = TCache
  { timeToLive :: NominalDiffTime
  , regenerateProc :: IO a
  , inFlight :: TVar Bool
  , tVal :: TVar (Maybe (TCacheVal a)) }

data TCacheVal a = TCacheVal
  { birthday :: UTCTime
  , value :: a }

newTCacheIO :: NominalDiffTime -> IO a -> IO (TCache a)
newTCacheIO ttl regen = do
  val <- newTVarIO Nothing
  inFlight <- newTVarIO False
  return TCache {timeToLive = ttl, regenerateProc = regen, inFlight, tVal = val}

cacheOK :: TCache a -> UTCTime -> STM Bool
cacheOK cache now = do
  val <- readTVar (tVal cache)
  case val of
    Just val' -> return $ diffUTCTime now (birthday val') < timeToLive cache
    Nothing   -> return False

readTCache :: TCache a -> UTCTime -> STM a
readTCache cache now = do
  val <- readTVar (tVal cache)
  case val of
    Just TCacheVal {value} -> do ok <- cacheOK cache now
                                 if ok then return value
                                       else regenTCache cache now
    Nothing -> regenTCache cache now

regenTCache :: TCache a -> UTCTime -> STM a
regenTCache cache birthday = do
  flying <- readTVar (inFlight cache)
  if flying then retry else do
    writeTVar (inFlight cache) True
    newVal <- unsafeIOToSTM (regenerateProc cache)
    writeTVar (tVal cache) (Just $ TCacheVal {value = newVal, birthday})
    writeTVar (inFlight cache) False
    return newVal
