module Podcast.Episodes.Episode5
  ( episode5
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

episode5 :: Either String Episode.Episode
episode5 =
  Episode.Episode
    <$> Article.fromString
          "https://sakshamsharma.com/2018/03/haskell-proj-struct/"
    <*> Date.fromGregorian 2019 4 8
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about build tools in Haskell, \
          \including Stack and Cabal."
    <*> Seconds.fromTimestamp 15 15
    <*> Guid.fromString "25b43cdb-e278-42da-97dc-3c6d353ec8c8"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-08-episode-5.mp3"
    <*> Number.fromNatural 5
    <*> pure (Bytes.fromNatural 21977225)
    <*> Title.fromString "Build tools"
    <*> pure Nothing
