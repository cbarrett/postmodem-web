{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

module Web.Postmodem.Feed
  ( getEpisodes
  , Episode, title, description, date, audio
  , Audio, url, _type
  ) where

import Data.Maybe
import Data.Time.Clock
import Network.Curl.Download
import Text.Feed.Query
import Text.Feed.Types

data Episode = Episode
  { title :: String
  , description :: String
  , date :: UTCTime
  , audio :: Audio
  } deriving (Show)

data Audio = Audio
  { url :: String
  , _type :: String
  } deriving (Show)

getEpisodes :: IO [Episode]
getEpisodes = openAsFeed "http://feeds.feedburner.com/postmodem" >>= return . either (const []) processFeed

processFeed :: Feed -> [Episode]
processFeed f = catMaybes $ for (feedItems f) $ \item -> do
  title <- getItemTitle item
  description <- getItemDescription item
  Just date <- getItemPublishDate item
  (audioURL, Just audioType, _) <- getItemEnclosure item
  return Episode { title, description, date, audio = Audio { url = audioURL, _type = audioType } }

for = flip fmap
