module Podcast.Episodes.Episode8
  ( episode8
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

episode8 :: Either String Episode.Episode
episode8 =
  Episode.Episode
    <$> Article.fromString
          "https://medium.com/co-star-engineering/continuous-improvement-with-hlint-code-smells-e490886558a1"
    <*> Date.fromGregorian 2019 4 29
    <*> Description.fromString
          "Cameron Gera and Cody Goodman talk about enforcing best practices \
          \with HLint and refactoring."
    <*> Seconds.fromTimestamp 14 20
    <*> Guid.fromString "53bbcaeb-6e6f-4e1f-9806-f24032ac7a9f"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-29-episode-8.mp3"
    <*> Number.fromNatural 8
    <*> Right (Bytes.fromNatural 20714874)
    <*> Title.fromString "Best practices"
    <*> Right Nothing
