module Main ( main ) where
import qualified Data.UUID as Uuid
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf

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
          , "<link>", episodeLink root episode, "</link>"
          , "<guid isPermalink='false'>", Uuid.toString (episodeGuid episode), "</guid>"
          , "<description>", episodeDescription episode, "</description>"
          , "<itunes:author>Taylor Fausak</itunes:author>"
          , "<enclosure "
            , "type='audio/mpeg' "
            , "length='", show (episodeSize episode), "' "
            , "url='", episodeUrl episode, "' />"
          , "<itunes:duration>"
            , formatDuration (episodeDuration episode)
          , "</itunes:duration>"
        , "</item>"
        ])
      [ Episode
        2
        "Sara Lichtenstein talks about upgrading Elm."
        "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
        21580339
        (14 * 60 + 59)
        (Uuid.fromWords 0x00900298 0x5aa64301 0xa207619d 0x38cdc81a)
      , Episode
        1
        "Cody Goodman talks about exceptions."
        "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
        13999481
        (9 * 60 + 43)
        (Uuid.fromWords 0x6fe12dba 0xe0c34af5 0xb9fc844b 0xc2396ae7)
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
        , "<itunes:author>Taylor Fausak</itunes:author>"
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
  , episodeDuration :: Integer
  , episodeGuid :: Uuid.UUID
  } deriving (Eq, Show)

episodeLink :: String -> Episode -> String
episodeLink root episode = concat
  [root, "#episode-", show (episodeNumber episode)]

formatDuration :: Integer -> String
formatDuration total =
  let (minutes, seconds) = quotRem total 60
  in Printf.printf "%d:%02d" minutes seconds
