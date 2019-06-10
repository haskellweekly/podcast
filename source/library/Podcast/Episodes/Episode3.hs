module Podcast.Episodes.Episode3
  ( episode3
  )
where

import qualified Podcast.Type.Article as Article
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title

episode3 :: Either String Episode.Episode
episode3 =
  Episode.Episode
    <$> Article.fromString
          "https://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html"
    <*> Date.fromGregorian 2019 3 25
    <*> Description.fromString
          "Jason Fry and Taylor Fausak compare frontend and backend \
          \languages, including PureScript and Elm."
    <*> Seconds.fromTimestamp 23 47
    <*> Guid.fromString "069964f7-2457-479f-8bab-9cb4f3abec9c"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-25-episode-3.mp3"
    <*> Number.fromNatural 3
    <*> Right (Bytes.fromNatural 34265398)
    <*> Title.fromString "Frontend languages"
    <*> Right Nothing
