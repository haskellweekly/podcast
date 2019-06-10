module Podcast.Episodes.Episode4
  ( episode4
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

episode4 :: Either String Episode.Episode
episode4 =
  Episode.Episode
    <$> Article.fromString
          "https://runtimeverification.com/blog/code-smell-boolean-blindness/"
    <*> Date.fromGregorian 2019 4 1
    <*> Description.fromString
          "Dustin Segers and Taylor Fausak talk about avoiding boolean \
          \blindness by using custom types."
    <*> Seconds.fromTimestamp 15 57
    <*> Guid.fromString "aea8101c-b126-4cb5-be14-00200d3f6c27"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-01-episode-4.mp3"
    <*> Number.fromNatural 4
    <*> pure (Bytes.fromNatural 23002958)
    <*> Title.fromString "Boolean blindness"
    <*> pure Nothing
