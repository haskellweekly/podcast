module Podcast.Episodes.Episode7
  ( episode7
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

episode7 :: Either String Episode.Episode
episode7 =
  Episode.Episode
    <$> Article.fromString
          "https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html"
    <*> Date.fromGregorian 2019 4 22
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how regular expressions \
          \compare to parser combinators in Haskell."
    <*> Seconds.fromTimestamp 17 29
    <*> Guid.fromString "287a197e-e9fd-47b6-9506-2f39be002af7"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-22-episode-7.mp3"
    <*> Number.fromNatural 7
    <*> pure (Bytes.fromNatural 25296111)
    <*> Title.fromString "Parser combinators"
    <*> Right Nothing
