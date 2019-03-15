module Main ( main ) where
import qualified Data.UUID as Uuid
import qualified Numeric.Natural as Natural
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
  writeFile (FilePath.combine output "haskell-weekly-podcast.rss") (concat
    [ "<?xml version='1.0' encoding='utf-8'?>"
    , "<rss version='2.0' xmlns:itunes='http://www.itunes.com/dtds/podcast-1.0.dtd'>"
      , "<channel>"
        , "<title>Haskell Weekly</title>"
        , "<link>", escapeString root, "</link>"
        , "<description>"
          , "Short, casual discussion about the Haskell programming language."
        , "</description>"
        , "<itunes:author>Taylor Fausak</itunes:author>"
        , "<language>en-US</language>"
        , "<itunes:category text='Technology' />"
        , "<image>"
          , "<title>Haskell Weekly</title>"
          , "<link>", escapeString root, "</link>"
          , "<url>", escapeString root, "/haskell-weekly-podcast.png</url>"
        , "</image>"
        , concatMap (formatEpisode root) episodes
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

formatEpisode :: String -> Episode -> String
formatEpisode root episode = concat
  [ "<item>"
    , "<title>", escapeString (formatNumber (episodeNumber episode)), "</title>"
    , "<link>", escapeString (episodeLink root episode), "</link>"
    , "<guid isPermalink='false'>", escapeString (Uuid.toString (episodeGuid episode)), "</guid>"
    , "<description>", escapeString (episodeDescription episode), "</description>"
    , "<itunes:author>Taylor Fausak</itunes:author>"
    , "<enclosure "
      , "type='audio/mpeg' "
      , "length='", escapeString (formatBytes (episodeSize episode)), "' "
      , "url='", escapeString (episodeUrl episode), "' />"
    , "<itunes:duration>"
      , escapeString (formatSeconds (episodeDuration episode))
    , "</itunes:duration>"
  , "</item>"
  ]

episodes :: [Episode]
episodes =
  [ Episode
    (Number 2)
    "Sara Lichtenstein talks about upgrading Elm."
    "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
    (Bytes 21580339)
    (Seconds 1019)
    (Uuid.fromWords 0x00900298 0x5aa64301 0xa207619d 0x38cdc81a)
  , Episode
    (Number 1)
    "Cody Goodman talks about exceptions."
    "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
    (Bytes 13999481)
    (Seconds 583)
    (Uuid.fromWords 0x6fe12dba 0xe0c34af5 0xb9fc844b 0xc2396ae7)
  ]

data Episode = Episode
  { episodeNumber :: Number
  , episodeDescription :: String
  , episodeUrl :: String
  , episodeSize :: Bytes
  , episodeDuration :: Seconds
  , episodeGuid :: Uuid.UUID
  } deriving (Eq, Show)

episodeLink :: String -> Episode -> String
episodeLink root episode = concat
  [root, "#episode-", formatNumber (episodeNumber episode)]

newtype Bytes = Bytes
  { unwrapBytes :: Natural.Natural
  } deriving (Eq, Show)

formatBytes :: Bytes -> String
formatBytes = show . unwrapBytes

newtype Seconds = Seconds
  { unwrapSeconds :: Natural.Natural
  } deriving (Eq, Show)

formatSeconds :: Seconds -> String
formatSeconds seconds =
  let (m, s) = quotRem (unwrapSeconds seconds) 60
  in Printf.printf "%d:%02d" m s

newtype Number = Number
  { unwrapNumber :: Natural.Natural
  } deriving (Eq, Show)

formatNumber :: Number -> String
formatNumber = show . unwrapNumber

escapeString :: String -> String
escapeString = concatMap escapeChar

escapeChar :: Char -> String
escapeChar c = case c of
  '\x22' -> "&quot;"
  '\x26' -> "&amp;"
  '\x27' -> "&apos;"
  '\x3c' -> "&lt;"
  '\x3e' -> "&gt;"
  _ -> [c]
