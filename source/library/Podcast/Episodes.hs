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
import qualified Podcast.Type.Date as Time
import qualified Podcast.Type.Url as Url

episodes :: [Either String Episode.Episode]
episodes =
  [ Episode.Episode
    <$> Time.fromGregorian 2019 5 6
    <*> Description.fromString
          "Jason Fry and Cameron Gera talk about four small ways to improve \
          \Haskell as a language.\n\n\
          \https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816"
    <*> Seconds.fromTimestamp 21 52
    <*> Guid.fromString "de704aad-e6a1-41a6-976f-bd3f2ef34ad2"
    <*> Number.fromNatural 9
    <*> pure (Bytes.fromNatural 31507647)
    <*> Url.fromString
          "https://user.fm/files/v2-c184bb75e7ebae3e6d24f15a08aeb557/episode-9.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 4 29
    <*> Description.fromString
          "Cameron Gera and Cody Goodman talk about enforcing best practices \
          \with HLint and refactoring.\n\n\
          \https://medium.com/co-star-engineering/continuous-improvement-with-hlint-code-smells-e490886558a1"
    <*> Seconds.fromTimestamp 14 20
    <*> Guid.fromString "346b385f-40e4-4938-aecc-d9d82de4aeec"
    <*> Number.fromNatural 8
    <*> pure (Bytes.fromNatural 20714874)
    <*> Url.fromString
          "https://user.fm/files/v2-a250a0ecdb53e9e3c36c4d57c7cceb21/episode-8.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 4 22
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how regular expressions \
          \compare to parser combinators in Haskell.\n\n\
          \https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html"
    <*> Seconds.fromTimestamp 17 29
    <*> Guid.fromString "287a197e-e9fd-47b6-9506-2f39be002af7"
    <*> Number.fromNatural 7
    <*> pure (Bytes.fromNatural 25296111)
    <*> Url.fromString
          "https://user.fm/files/v2-7f984b030cc6c2f46fb6b4b78e802d4c/episode-7.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 4 15
    <*> Description.fromString
          "Jason Fry and Taylor Fausak talk about getting fast feedback with \
          \ghcid.\n\n\
          \https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev"
    <*> Seconds.fromTimestamp 18 38
    <*> Guid.fromString "7ed15199-bcd3-461e-af62-d504ae8a4a01"
    <*> Number.fromNatural 6
    <*> pure (Bytes.fromNatural 26845627)
    <*> Url.fromString
          "https://user.fm/files/v2-fb17e86f19ce358097a1c11c1276733d/episode-6.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 4 08
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about build tools in Haskell, \
          \including Stack and Cabal.\n\n\
          \https://sakshamsharma.com/2018/03/haskell-proj-struct/"
    <*> Seconds.fromTimestamp 15 15
    <*> Guid.fromString "25b43cdb-e278-42da-97dc-3c6d353ec8c8"
    <*> Number.fromNatural 5
    <*> pure (Bytes.fromNatural 21977225)
    <*> Url.fromString
          "https://user.fm/files/v2-5566df6d67fc930b0e1fdece8b231918/episode-5.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 4 1
    <*> Description.fromString
          "Dustin Segers and Taylor Fausak talk about avoiding boolean \
          \blindness by using custom types.\n\n\
          \https://runtimeverification.com/blog/code-smell-boolean-blindness/"
    <*> Seconds.fromTimestamp 15 57
    <*> Guid.fromString "aea8101c-b126-4cb5-be14-00200d3f6c27"
    <*> Number.fromNatural 4
    <*> pure (Bytes.fromNatural 23002958)
    <*> Url.fromString
          "https://user.fm/files/v2-1323dfdd0d35e682971a2bbeb5dc3a9e/episode-4.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 3 25
    <*> Description.fromString
          "Jason Fry and Taylor Fausak talk which languages to use on the \
          \frontend and backend, including Haskell, PureScript, and Elm.\n\n\
          \https://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html"
    <*> Seconds.fromTimestamp 23 47
    <*> Guid.fromString "069964f7-2457-479f-8bab-9cb4f3abec9c"
    <*> Number.fromNatural 3
    <*> pure (Bytes.fromNatural 34265398)
    <*> Url.fromString
          "https://user.fm/files/v2-7223322ce11c6c3ec69111784aa34893/episode-3.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 3 18
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about upgrading from Elm \
          \0.18 to 0.19.\n\n\
          \https://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> Seconds.fromTimestamp 14 59
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Number.fromNatural 2
    <*> pure (Bytes.fromNatural 21580339)
    <*> Url.fromString
          "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
  , Episode.Episode
    <$> Time.fromGregorian 2019 3 11
    <*> Description.fromString
          "Cody Goodman and Taylor Fausak talk about handling exceptions in \
          \Haskell.\n\n\
          \https://markkarpov.com/tutorial/exceptions.html"
    <*> Seconds.fromTimestamp 9 43
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Number.fromNatural 1
    <*> pure (Bytes.fromNatural 13999481)
    <*> Url.fromString
          "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
  ]
