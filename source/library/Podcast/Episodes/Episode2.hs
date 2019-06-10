module Podcast.Episodes.Episode2
  ( episode2
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

episode2 :: Either String Episode.Episode
episode2 =
  Episode.Episode
    <$> Article.fromString
          "https://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> Date.fromGregorian 2019 3 18
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about the good and bad of \
          \upgrading from Elm 0.18 to 0.19."
    <*> Seconds.fromTimestamp 14 59
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-18-episode-2.mp3"
    <*> Number.fromNatural 2
    <*> Right (Bytes.fromNatural 21580339)
    <*> Title.fromString "Upgrading Elm"
    <*> Right Nothing
