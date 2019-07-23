module Podcast.Episodes.Episode15
  ( episode15
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

episode15 :: Either String Episode.Episode
episode15 =
  Episode.Episode
    <$> Article.fromString "https://treszkai.github.io/2019/07/13/haskell-eval"
    <*> Date.fromGregorian 2019 7 23
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how function calls are \
          \evaluated in Haskell with regards to non-strictness."
    <*> Seconds.fromTimestamp 18 13
    <*> Guid.fromString "a76ba20a-49f7-4a5f-a40d-bffb34417b2d"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-07-23-episode-15.mp3"
    <*> Number.fromNatural 15
    <*> Right (Bytes.fromNatural 26208359)
    <*> Title.fromString "Lazy sharing"
    <*> Right Nothing
