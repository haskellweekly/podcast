module Podcast.Episodes.Episode14
  ( episode14
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

episode14 :: Either String Episode.Episode
episode14 =
  Episode.Episode
    <$> Article.fromString
          "https://danieljharvey.github.io/posts/2019-07-05-refined-types.html"
    <*> Date.fromGregorian 2019 7 16
    <*> Description.fromString
          "Andres Schmois and Cody Goodman talk about using the Refined \
          \library to turn runtime checks into types."
    <*> Seconds.fromTimestamp 15 18
    <*> Guid.fromString "5ec19b21-9399-474b-be54-beadd37894f9"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-07-16-episode-14.mp3"
    <*> Number.fromNatural 14
    <*> Right (Bytes.fromNatural 22040576)
    <*> Title.fromString "Refinement types"
    <*> Right Nothing
