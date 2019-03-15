module Main ( main ) where
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

main :: IO ()
main = do
  let
    url = "https://haskellweekly.news/podcast"
    input = "input"
    output = "output"
  Directory.createDirectoryIfMissing True output

  Directory.copyFile
    (FilePath.combine input "logo.png")
    (FilePath.combine output "haskell-weekly-podcast.png")

  -- https://help.apple.com/itc/podcasts_connect/#/itcbaf351599
  let
    items = concatMap
      (\ episode -> concat
        [ "<item>"
          , "<title>", show (episodeNumber episode), "</title>"
          , "<link>", episodeLink episode, "</link>"
          , "<guid>", url, "#episode-", show (episodeNumber episode), "</guid>"
          , "<description>", episodeDescription episode, "</description>"
        , "</item>"
        ])
      [ Episode
        2
        "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
        "Sara Lichtenstein and Taylor Fausak talk about upgrading Elm."
      , Episode
        1
        "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
        "Cody Goodman and Taylor Fausak talk about exceptions."
      ]
  writeFile (FilePath.combine output "haskell-weekly-podcast.rss") (concat
    [ "<?xml version='1.0' encoding='utf-8'?>"
    , "<rss version='2.0'>"
      , "<channel>"
        , "<title>Haskell Weekly</title>"
        , "<link>", url, "</link>"
        , "<description>"
          , "Short, casual discussion about the Haskell programming language."
        , "</description>"
        , "<image>"
          , "<title>Haskell Weekly</title>"
          , "<link>", url, "</link>"
          , "<url>", url, "/haskell-weekly-podcast.png</url>"
        , "</image>"
        , items
      , "</channel>"
    , "</rss>"
    ])

  writeFile (FilePath.combine output "index.html") (concat
    [ "<!doctype html>"
    , "<html>"
      , "<head>"
        , "<meta charset='utf-8'>"
        , "<title>Haskell Weekly Podcast</title>"
      , "<head>"
      , "</head>"
      , "<body>"
        , "<h1>Haskell Weekly Podcast</h1>"
        , "<p>"
          , "<a href='haskell-weekly-podcast.rss'>RSS feed</a>"
        , "</p>"
      , "</body>"
    , "</html>"
    ])

data Episode = Episode
  { episodeNumber :: Integer
  , episodeLink :: String
  , episodeDescription :: String
  } deriving (Eq, Show)
