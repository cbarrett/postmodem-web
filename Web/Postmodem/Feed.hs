{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

module Web.Postmodem.Feed
  ( getEpisodes
  , Episode (..)
  , Audio (..)
  ) where

import Data.Maybe
import Data.Time.Clock
import Network.Curl.Download
import Text.Feed.Query
import Text.Feed.Types
import Data.Text
import Control.Applicative

data Episode = Episode
  { epTitle :: Text
  , description :: Text
  , date :: UTCTime
  , epAudio :: Audio
  } deriving (Show)

data Audio = Audio
  { url :: Text
  , _type :: Text
  } deriving (Show)

getEpisodes :: IO [Episode]
getEpisodes = openAsFeed "http://feeds.feedburner.com/postmodem" >>= return . either (const []) processFeed

processFeed :: Feed -> [Episode]
processFeed f = catMaybes $ for (feedItems f) $ \item -> do
  title <- pack <$> getItemTitle item
  description <- pack <$> getItemDescription item
  Just date <- getItemPublishDate item
  (audioURL, Just audioType, _) <- getItemEnclosure item
  return Episode { epTitle = title, description, date, epAudio = Audio { url = pack audioURL, _type = pack audioType } }

for = flip fmap
