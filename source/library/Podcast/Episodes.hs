module Podcast.Episodes
  ( episodes
  )
where

import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Time as Time
import qualified Podcast.Type.Url as Url

episodes :: [Either String Episode.Episode]
episodes =
  [ Episode.Episode
    <$> Description.fromString "Sara Lichtenstein and Taylor Fausak talk about upgrading from Elm 0.18 to 0.19.\n\nhttps://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> pure (Seconds.fromNatural 899)
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Number.fromNatural 2
    <*> pure (Bytes.fromNatural 21580339)
    <*> Time.fromString "2019-03-13T12:00:00"
    <*> Url.fromString "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
  , Episode.Episode
    <$> Description.fromString "Cody Goodman and Taylor Fausak talk about handling exceptions in Haskell.\n\nhttps://markkarpov.com/tutorial/exceptions.html"
    <*> pure (Seconds.fromNatural 583)
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Number.fromNatural 1
    <*> pure (Bytes.fromNatural 13999481)
    <*> Time.fromString "2019-03-06T12:00:00"
    <*> Url.fromString "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
  ]
