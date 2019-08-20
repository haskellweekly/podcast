module Podcast.Episodes.Episode19
  ( episode19
  )
where

import qualified Podcast.Type.Articles as Articles
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title

episode19 :: Either String Episode.Episode
episode19 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://thomashoneyman.com/articles/practical-profunctor-lenses-optics/"]
    <*> Date.fromGregorian 2019 8 20
    <*> Description.fromString
          "Cameron Gera and Andres Schmois talk about practical uses for \
          \profunctor lenses and optics."
    <*> Seconds.fromTimestamp 18 56
    <*> Guid.fromString "bbd19a71-7d79-4514-8acb-a5729cdd26c6"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-08-20-episode-19.mp3"
    <*> Number.fromNatural 19
    <*> Right (Bytes.fromNatural 27317566)
    <*> Title.fromString "Profunctor Optics"
    <*> Right Nothing
