module Podcast.Episodes.Episode10
  ( episode10
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

episode10 :: Either String Episode.Episode
episode10 =
  Episode.Episode
    <$> Article.fromString
          "https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/"
    <*> Date.fromGregorian 2019 5 20
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how Haskell encourages \
          \you to use the ports and adapters architecture."
    <*> Seconds.fromTimestamp 16 37
    <*> Guid.fromString "32fd3459-b349-4c99-9150-5073fedab6bf"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-20-episode-10.mp3"
    <*> Number.fromNatural 10
    <*> pure (Bytes.fromNatural 23942886)
    <*> Title.fromString "Functional architecture"
    <*> Right Nothing
