module Podcast.Episodes
  ( episodes
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

episodes :: [Either String Episode.Episode]
episodes =
  [ Episode.Episode
    <$> Article.fromString "https://www.tweag.io/posts/2019-05-27-ormolu.html"
    <*> Date.fromGregorian 2019 6 3
    <*> Description.fromString
          "Dustin Segers and Cody Goodman talk about formatting Haskell \
          \source code with automated tools like Ormolu.\n\n\
          \https://www.tweag.io/posts/2019-05-27-ormolu.html"
    <*> Seconds.fromTimestamp 16 37
    <*> Guid.fromString "f166f89f-1a16-49f1-915a-d54505c301a0"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-06-03-episode-12.mp3"
    <*> Number.fromNatural 12
    <*> pure (Bytes.fromNatural 23912963)
    <*> Title.fromString "Formatting code"
  , Episode.Episode
    <$> Article.fromString "https://blog.jez.io/profiling-in-haskell/"
    <*> Date.fromGregorian 2019 5 27
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about improving the \
          \performance of Haskell programs by profiling them.\n\n\
          \https://blog.jez.io/profiling-in-haskell/"
    <*> Seconds.fromTimestamp 19 12
    <*> Guid.fromString "3ec1dc79-7a9c-46c3-b919-61471e876708"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-27-episode-11.mp3"
    <*> Number.fromNatural 11
    <*> pure (Bytes.fromNatural 27690623)
    <*> Title.fromString "Profiling performance"
  , Episode.Episode
    <$> Article.fromString
          "https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/"
    <*> Date.fromGregorian 2019 5 20
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how Haskell encourages \
          \you to use the ports and adapters architecture.\n\n\
          \https://blog.ploeh.dk/2016/03/18/functional-architecture-is-ports-and-adapters/"
    <*> Seconds.fromTimestamp 16 37
    <*> Guid.fromString "32fd3459-b349-4c99-9150-5073fedab6bf"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-20-episode-10.mp3"
    <*> Number.fromNatural 10
    <*> pure (Bytes.fromNatural 23942886)
    <*> Title.fromString "Functional architecture"
  , Episode.Episode
    <$> Article.fromString
          "https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816"
    <*> Date.fromGregorian 2019 5 6
    <*> Description.fromString
          "Jason Fry and Cameron Gera talk about four small ways to improve \
          \Haskell as a language.\n\n\
          \https://medium.com/daml-driven/four-tweaks-to-improve-haskell-b1de9c87f816"
    <*> Seconds.fromTimestamp 21 52
    <*> Guid.fromString "de704aad-e6a1-41a6-976f-bd3f2ef34ad2"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-05-06-episode-9.mp3"
    <*> Number.fromNatural 9
    <*> pure (Bytes.fromNatural 31507647)
    <*> Title.fromString "Improving Haskell"
  , Episode.Episode
    <$> Article.fromString
          "https://medium.com/co-star-engineering/continuous-improvement-with-hlint-code-smells-e490886558a1"
    <*> Date.fromGregorian 2019 4 29
    <*> Description.fromString
          "Cameron Gera and Cody Goodman talk about enforcing best practices \
          \with HLint and refactoring.\n\n\
          \https://medium.com/co-star-engineering/continuous-improvement-with-hlint-code-smells-e490886558a1"
    <*> Seconds.fromTimestamp 14 20
    <*> Guid.fromString "53bbcaeb-6e6f-4e1f-9806-f24032ac7a9f"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-29-episode-8.mp3"
    <*> Number.fromNatural 8
    <*> pure (Bytes.fromNatural 20714874)
    <*> Title.fromString "Best practices"
  , Episode.Episode
    <$> Article.fromString
          "https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html"
    <*> Date.fromGregorian 2019 4 22
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about how regular expressions \
          \compare to parser combinators in Haskell.\n\n\
          \https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html"
    <*> Seconds.fromTimestamp 17 29
    <*> Guid.fromString "287a197e-e9fd-47b6-9506-2f39be002af7"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-22-episode-7.mp3"
    <*> Number.fromNatural 7
    <*> pure (Bytes.fromNatural 25296111)
    <*> Title.fromString "Parser combinators"
  , Episode.Episode
    <$> Article.fromString
          "https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev"
    <*> Date.fromGregorian 2019 4 15
    <*> Description.fromString
          "Jason Fry and Taylor Fausak talk about getting fast feedback when \
          \developing Haskell by using ghcid.\n\n\
          \https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev"
    <*> Seconds.fromTimestamp 18 38
    <*> Guid.fromString "7ed15199-bcd3-461e-af62-d504ae8a4a01"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-15-episode-6.mp3"
    <*> Number.fromNatural 6
    <*> pure (Bytes.fromNatural 26845627)
    <*> Title.fromString "Fast feedback"
  , Episode.Episode
    <$> Article.fromString
          "https://sakshamsharma.com/2018/03/haskell-proj-struct/"
    <*> Date.fromGregorian 2019 4 8
    <*> Description.fromString
          "Cameron Gera and Taylor Fausak talk about build tools in Haskell, \
          \including Stack and Cabal.\n\n\
          \https://sakshamsharma.com/2018/03/haskell-proj-struct/"
    <*> Seconds.fromTimestamp 15 15
    <*> Guid.fromString "25b43cdb-e278-42da-97dc-3c6d353ec8c8"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-08-episode-5.mp3"
    <*> Number.fromNatural 5
    <*> pure (Bytes.fromNatural 21977225)
    <*> Title.fromString "Build tools"
  , Episode.Episode
    <$> Article.fromString
          "https://runtimeverification.com/blog/code-smell-boolean-blindness/"
    <*> Date.fromGregorian 2019 4 1
    <*> Description.fromString
          "Dustin Segers and Taylor Fausak talk about avoiding boolean \
          \blindness by using custom types.\n\n\
          \https://runtimeverification.com/blog/code-smell-boolean-blindness/"
    <*> Seconds.fromTimestamp 15 57
    <*> Guid.fromString "aea8101c-b126-4cb5-be14-00200d3f6c27"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-04-01-episode-4.mp3"
    <*> Number.fromNatural 4
    <*> pure (Bytes.fromNatural 23002958)
    <*> Title.fromString "Boolean blindness"
  , Episode.Episode
    <$> Article.fromString
          "https://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html"
    <*> Date.fromGregorian 2019 3 25
    <*> Description.fromString
          "Jason Fry and Taylor Fausak compare frontend and backend \
          \languages, including PureScript and Elm.\n\n\
          \https://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html"
    <*> Seconds.fromTimestamp 23 47
    <*> Guid.fromString "069964f7-2457-479f-8bab-9cb4f3abec9c"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-25-episode-3.mp3"
    <*> Number.fromNatural 3
    <*> pure (Bytes.fromNatural 34265398)
    <*> Title.fromString "Frontend languages"
  , Episode.Episode
    <$> Article.fromString
          "https://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> Date.fromGregorian 2019 3 18
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak talk about the good and bad of \ \upgrading from Elm 0.18 to 0.19.\n\n\
          \https://engineering.itpro.tv/2019/03/01/upgrading-elm-to-v19/"
    <*> Seconds.fromTimestamp 14 59
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-18-episode-2.mp3"
    <*> Number.fromNatural 2
    <*> pure (Bytes.fromNatural 21580339)
    <*> Title.fromString "Upgrading Elm"
  , Episode.Episode
    <$> Article.fromString "https://markkarpov.com/tutorial/exceptions.html"
    <*> Date.fromGregorian 2019 3 11
    <*> Description.fromString
          "Cody Goodman and Taylor Fausak talk about handling errors in \
          \Haskell by using exceptions.\n\n\
          \https://markkarpov.com/tutorial/exceptions.html"
    <*> Seconds.fromTimestamp 9 43
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-03-11-episode-1.mp3"
    <*> Number.fromNatural 1
    <*> pure (Bytes.fromNatural 13999481)
    <*> Title.fromString "Handling exceptions"
  ]
