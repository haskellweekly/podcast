module Podcast.Episodes.Episode17
  ( episode17
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

episode17 :: Either String Episode.Episode
episode17 =
  Episode.Episode
    <$> Articles.fromStrings
          [ "https://typeclasses.com/news/2019-07-phrasebook"
          ]
    <*> Date.fromGregorian 2019 8 6
    <*> Description.fromString
          "Sara Lichtenstein and Andres Schmois discuss quickly learning \
          \Haskell by studying annotated examples."
    <*> Seconds.fromTimestamp 13 56
    <*> Guid.fromString "df526ec2-5d4e-4c1a-b4b5-eca8b6918731"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-08-06-episode-17.mp3"
    <*> Number.fromNatural 17
    <*> Right (Bytes.fromNatural 20120892)
    <*> Title.fromString "Haskell Phrasebook"
    <*> Right Nothing
