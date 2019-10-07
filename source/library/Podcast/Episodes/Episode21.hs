module Podcast.Episodes.Episode21
  ( episode21
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

episode21 :: Either String Episode.Episode
episode21 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://www.well-typed.com/blog/2019/09/eventful-ghc/"]
    <*> Date.fromGregorian 2019 10 7
    <*> Description.fromString
          "Cody Goodman and Taylor Fausak explore the event log that GHC can produce when compiling or running."
    <*> Seconds.fromTimestamp 14 46
    <*> Guid.fromString "dc731681-016f-4380-8acc-18877ba41abe"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-10-07-episode-21.mp3"
    <*> Number.fromNatural 21
    <*> Right (Bytes.fromNatural 21299200)
    <*> Title.fromString "Event Log"
    <*> Right Nothing
