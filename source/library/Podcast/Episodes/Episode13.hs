module Podcast.Episodes.Episode13
  ( episode13
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

episode13 :: Either String Episode.Episode
episode13 =
  Episode.Episode
    <$> Article.fromString
          "https://github.com/github/semantic/blob/eaf13783838861fe5eb6cd46d59354774a8eb88d/docs/why-haskell.md"
    <*> Date.fromGregorian 2019 6 10
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about why the Semantic team at \
          \GitHub decided to use Haskell."
    <*> Seconds.fromTimestamp 25 8
    <*> Guid.fromString "fb192c3c-02a5-4413-ab53-1346677940ec"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-06-10-episode-13.mp3"
    <*> Number.fromNatural 13
    <*> Right (Bytes.fromNatural 26111814)
    <*> Title.fromString "Why Haskell?"
    <*> Right Nothing
