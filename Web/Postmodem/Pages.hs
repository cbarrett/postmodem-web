{-# LANGUAGE OverloadedStrings #-}

module Web.Postmodem.Pages where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5 as A
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Web.Postmodem.Feed
import qualified Web.Postmodem.Feed as W

index :: [Episode] -> Html
index episodes = docTypeHtml $ do
  H.head $ do
    H.title "Postmodem"
    link ! href "style.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "http://feeds.feedburner.com/postmodem" ! rel "alternate" ! type_ "application/rss+xml"
  body $ do
    header $ do
      h1 $ do
        a ! href "/" $ "Postmodem"
      h2 "Voted World's Sincerest Podcast Three Years Running"
      section $ do
        p ! class_ "intro" $ "Three bright-eyed, sincere twenty-somethings discuss pop culture, technology, and rap music."
        p "Hosted by Colin Barrett, Patrick Thomson, and Phillip Bowden."
        ul $ do
          li $ a ! href "http://feeds.feedburner.com/postmodem" $ "Subscribe with RSS"
          li $ a ! href "https://itunes.apple.com/us/podcast/postmodem/id530525508" $ "Subscribe with iTunes"
          li $ a ! href "https://twitter.com/postmodemcast" $ "Follow us on Twitter"
    
    let episodesWithIndex = zip episodes [1..]
    foldMap episodeFragment episodesWithIndex

tshow :: (Show a) => a -> Text
tshow = T.pack . show
    
episodeTitle :: Int -> Episode -> Text
episodeTitle i ep = "Postmodem: #" <> (tshow i) <> "— " <> (epTitle ep)
    
episodeURL :: Int -> Text
episodeURL i = "episode/" <> (tshow i)

episodeFragment :: (Episode, Int) -> Html
episodeFragment (ep, index) = article ! class_ "episode" $ do
  header $ do
    h1 $ do
      a ! href (toValue $ episodeURL index) $ toHtml (epTitle ep)
      H.span ! class_ "date" $ toHtml (show (date ep))
    section $ do
      p $ preEscapedToHtml (description ep)

episode :: Int -> Episode -> Html
episode index ep = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml $ episodeTitle index ep
    link ! href "style.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "http://feeds.feedburner.com/postmodem" ! rel "alternate" ! type_ "application/rss+xml"
  body $ do
    header $ do
      h1 $ do
        a ! href "/" $ "Postmodem"
      h2 "Voted World's Sincerest Podcast Three Years Running"
      header $ do
        h1 $ do
          a ! href (toValue $ episodeURL index) $ toHtml (epTitle ep)
          H.span ! class_ "date" $ toHtml (show (date ep))
      section $ do
        preEscapedToHtml (description ep)
        audio ! controls "" $ do
          let s = toValue $ url $ epAudio ep
          source ! src s ! type_ "audio/mpeg"