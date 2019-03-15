module Main ( main ) where
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

main :: IO ()
main = do
  let
    root = "https://haskellweekly.news/podcast"
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
          , "<link>", episodeGuid root episode, "</link>"
          , "<guid>", episodeGuid root episode, "</guid>"
          , "<description>", episodeDescription episode, "</description>"
          , "<author>Taylor Fausak</author>"
          , "<enclosure "
            , "type='audio/mpeg' "
            , "length='", show (episodeSize episode), "' "
            , "url='", episodeUrl episode, "' />"
        , "</item>"
        ])
      [ Episode
        2
        "Sara Lichtenstein talks about upgrading Elm."
        "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
        21580339
      , Episode
        1
        "Cody Goodman talks about exceptions."
        "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
        13999481
      ]
  writeFile (FilePath.combine output "haskell-weekly-podcast.rss") (concat
    [ "<?xml version='1.0' encoding='utf-8'?>"
    , "<rss version='2.0' xmlns:itunes='http://www.itunes.com/dtds/podcast-1.0.dtd'>"
      , "<channel>"
        , "<title>Haskell Weekly</title>"
        , "<link>", root, "</link>"
        , "<description>"
          , "Short, casual discussion about the Haskell programming language."
        , "</description>"
        , "<author>Taylor Fausak</author>"
        , "<language>en-US</language>"
        , "<itunes:category text='Technology' />"
        , "<image>"
          , "<title>Haskell Weekly</title>"
          , "<link>", root, "</link>"
          , "<url>", root, "/haskell-weekly-podcast.png</url>"
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
  , episodeDescription :: String
  , episodeUrl :: String
  , episodeSize :: Integer
  } deriving (Eq, Show)

episodeGuid :: String -> Episode -> String
episodeGuid root episode = concat
  [root, "#episode-", show (episodeNumber episode)]
