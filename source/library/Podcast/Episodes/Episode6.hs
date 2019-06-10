module Podcast.Episodes.Episode6
  ( episode6
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

episode6 :: Either String Episode.Episode
episode6 =
  Episode.Episode
    <$> Article.fromString
          "https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev"
    <*> Date.fromGregorian 2019 4 15
    <*> Description.fromString
          "Jason Fry and Taylor Fausak talk about getting fast feedback when \
          \developing Haskell by using ghcid."
    <*> Seconds.fromTimestamp 18 38
    <*> Guid.fromString "7ed15199-bcd3-461e-af62-d504ae8a4a01"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-15-episode-6.mp3"
    <*> Number.fromNatural 6
    <*> Right (Bytes.fromNatural 26845627)
    <*> Title.fromString "Fast feedback"
    <*> Right Nothing
