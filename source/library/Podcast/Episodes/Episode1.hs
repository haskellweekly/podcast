module Podcast.Episodes.Episode1
  ( episode1
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

episode1 :: Either String Episode.Episode
episode1 =
  Episode.Episode
    <$> Article.fromString "https://markkarpov.com/tutorial/exceptions.html"
    <*> Date.fromGregorian 2019 3 11
    <*> Description.fromString
          "Cody Goodman and Taylor Fausak talk about handling errors in \
          \Haskell by using exceptions."
    <*> Seconds.fromTimestamp 9 43
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-11-episode-1.mp3"
    <*> Number.fromNatural 1
    <*> pure (Bytes.fromNatural 13999481)
    <*> Title.fromString "Handling exceptions"
    <*> Right Nothing
