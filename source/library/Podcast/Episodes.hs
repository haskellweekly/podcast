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
  --   <$> Description.fromString
  --         "Cameron Gera and Taylor Fausak talk about build tools in Haskell, \
  --         \including Stack and Cabal.\n\n\
  --         \https://sakshamsharma.com/2018/03/haskell-proj-struct/"
  --   <*> pure (Seconds.fromNatural 915)
  --   <*> Guid.fromString "25b43cdb-e278-42da-97dc-3c6d353ec8c8"
  --   <*> Number.fromNatural 5
  --   <*> pure (Bytes.fromNatural 21977225)
  --   <*> Time.fromIso8601 "2019-04-08T12:00:00"
  --   <*> Url.fromString
  --         "https://user.fm/files/v2-5566df6d67fc930b0e1fdece8b231918/episode-5.mp3"
  -- , Episode.Episode
    <$> Description.fromString
          "Dustin Segers and Taylor Fausak talk about avoiding boolean \
          \blindness by using custom types.\n\n\
          \https://runtimeverification.com/blog/code-smell-boolean-blindness/"
    <*> pure (Seconds.fromNatural 957)
    <*> Guid.fromString "aea8101c-b126-4cb5-be14-00200d3f6c27"
    <*> Number.fromNatural 4
    <*> pure (Bytes.fromNatural 23002958)
    <*> Time.fromIso8601 "2019-04-01T12:00:00"
    <*> Url.fromString
          "https://user.fm/files/v2-1323dfdd0d35e682971a2bbeb5dc3a9e/episode-4.mp3"
  , Episode.Episode
    <$> Description.fromString
          "Jason Fry and Taylor Fausak talk which languages to use on the \
          \frontend and backend, including Haskell, PureScript, and Elm.\n\n\
          \https://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html"
    <*> pure (Seconds.fromNatural 1426)
    <*> Guid.fromString "069964f7-2457-479f-8bab-9cb4f3abec9c"
    <*> Number.fromNatural 3
    <*> pure (Bytes.fromNatural 34265398)
    <*> Time.fromIso8601 "2019-03-25T12:00:00"
    <*> Url.fromString
          "https://user.fm/files/v2-7223322ce11c6c3ec69111784aa34893/episode-3.mp3"
  , Episode.Episode
    <$> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about upgrading from Elm \
          \0.18 to 0.19.\n\n\
          \https://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> pure (Seconds.fromNatural 899)
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Number.fromNatural 2
    <*> pure (Bytes.fromNatural 21580339)
    <*> Time.fromIso8601 "2019-03-18T12:00:00"
    <*> Url.fromString
          "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
  , Episode.Episode
    <$> Description.fromString
          "Cody Goodman and Taylor Fausak talk about handling exceptions in \
          \Haskell.\n\n\
          \https://markkarpov.com/tutorial/exceptions.html"
    <*> pure (Seconds.fromNatural 583)
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Number.fromNatural 1
    <*> pure (Bytes.fromNatural 13999481)
    <*> Time.fromIso8601 "2019-03-11T12:00:00"
    <*> Url.fromString
          "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
  ]
