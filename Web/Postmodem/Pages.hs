{-# LANGUAGE OverloadedStrings #-}

module Web.Postmodem.Pages where

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5 as A
import Data.Foldable
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Control.Monad

import Web.Postmodem.Feed
import qualified Web.Postmodem.Feed as W

index :: [Episode] -> Html
index episodes = docTypeHtml $ do
  H.head $ do
    H.title "Postmodem"
    link ! href "/style.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "http://feeds.feedburner.com/postmodem" ! rel "alternate" ! type_ "application/rss+xml"
  body $ do
    header $ do
      h1 $ do
        a ! href "/" $ "Postmodem"
      h2 "Voted Most Sincere Podcast Three Years Running"
      section $ do
        p ! class_ "intro" $ "Three bright-eyed, sincere twenty-somethings discuss pop culture, technology, and rap music."
        p "Hosted by Colin Barrett, Patrick Thomson, and Phillip Bowden."
        p $ do
          a ! href "http://feeds.feedburner.com/postmodem" $ "Subscribe with RSS"
          " - "
          a ! href "https://itunes.apple.com/us/podcast/postmodem/id530525508" $ "Subscribe with iTunes"
          " - "
          a ! href "https://twitter.com/postmodemcast" $ "Follow us on Twitter"

    section $ do
      let episodesWithIndex = zip [1..] episodes 
      foldMap (uncurry $ episodeFragment False) episodesWithIndex
    
episodeTitle :: Int -> Episode -> Html
episodeTitle _ ep = "Postmodem" <> ndash <> " " <> toHtml (epTitle ep)
    where ndash = preEscapedToHtml ("&ndash;" :: Text)
episodeURL :: Int -> Text
episodeURL i = toLazyText $ "episode/" <> decimal i

episodeFragment :: Bool -> Int -> Episode -> Html
episodeFragment detail index ep = article ! class_ "episode" $ do
  article ! class_ "episode" $ do
    header $ do
      h1 $ do
        a ! href (toValue $ episodeURL index) $ toHtml (epTitle ep)
        H.span ! class_ "date" $ toHtml (show (date ep))
    section $ do
      p $ preEscapedToHtml (description ep)
      when detail $ do
        audio ! controls "" $ do
          let s = toValue $ url $ epAudio ep
          source ! src s ! type_ "audio/mpeg"

episode :: Int -> Episode -> Html
episode index ep = docTypeHtml $ do
  H.head $ do
    H.title $ episodeTitle index ep
    link ! href "/style.css" ! rel "stylesheet" ! type_ "text/css"
    link ! href "http://feeds.feedburner.com/postmodem" ! rel "alternate" ! type_ "application/rss+xml"
  body $ do
    header $ do
      h1 $ do
        a ! href "/" $ "Postmodem"
      h2 "Voted Most Sincere Podcast Three Years Running"    
    section $ episodeFragment True index ep
