module Podcast.Episodes.Episode12
  ( episode12
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

episode12 :: Either String Episode.Episode
episode12 =
  Episode.Episode
    <$> Article.fromString "https://www.tweag.io/posts/2019-05-27-ormolu.html"
    <*> Date.fromGregorian 2019 6 3
    <*> Description.fromString
          "Dustin Segers and Cody Goodman talk about formatting Haskell \
          \source code with automated tools like Ormolu."
    <*> Seconds.fromTimestamp 16 37
    <*> Guid.fromString "f166f89f-1a16-49f1-915a-d54505c301a0"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-06-03-episode-12.mp3"
    <*> Number.fromNatural 12
    <*> pure (Bytes.fromNatural 23912963)
    <*> Title.fromString "Formatting code"
    <*> Right Nothing
