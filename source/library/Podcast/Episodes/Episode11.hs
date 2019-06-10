module Podcast.Episodes.Episode11
  ( episode11
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

episode11 :: Either String Episode.Episode
episode11 =
  Episode.Episode
    <$> Article.fromString "https://blog.jez.io/profiling-in-haskell/"
    <*> Date.fromGregorian 2019 5 27
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about improving the \
          \performance of Haskell programs by profiling them."
    <*> Seconds.fromTimestamp 19 12
    <*> Guid.fromString "3ec1dc79-7a9c-46c3-b919-61471e876708"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-27-episode-11.mp3"
    <*> Number.fromNatural 11
    <*> Right (Bytes.fromNatural 27690623)
    <*> Title.fromString "Profiling performance"
    <*> Right Nothing
