module Podcast.Episodes.Episode9
  ( episode9
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

episode9 :: Either String Episode.Episode
episode9 =
  Episode.Episode
    <$> Article.fromString
          "https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816"
    <*> Date.fromGregorian 2019 5 6
    <*> Description.fromString
          "Jason Fry and Cameron Gera talk about four small ways to improve \
          \Haskell as a language."
    <*> Seconds.fromTimestamp 21 52
    <*> Guid.fromString "de704aad-e6a1-41a6-976f-bd3f2ef34ad2"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-06-episode-9.mp3"
    <*> Number.fromNatural 9
    <*> pure (Bytes.fromNatural 31507647)
    <*> Title.fromString "Improving Haskell"
    <*> pure Nothing
