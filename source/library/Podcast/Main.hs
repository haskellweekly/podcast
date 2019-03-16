module Podcast.Main
  ( defaultMain
  )
where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Network.URI as Uri
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf

defaultMain :: IO ()
defaultMain = do
  root <- either fail pure (parseUri "https://haskellweekly.news/podcast")
  episodes <- either fail pure (sequence episodeDefinitions)
  let
    input = "input"
    output = "output"
  Directory.createDirectoryIfMissing True output

  Directory.copyFile
    (FilePath.combine input "logo.png")
    (FilePath.combine output "logo.png")

  let directory = FilePath.combine output "episodes"
  Directory.createDirectoryIfMissing True directory
  mapM_
    (\ episode -> writeFile
      (FilePath.combine
        directory
        (FilePath.addExtension (formatNumber (episodeNumber episode)) "html"))
      (concat
        [ "<!doctype html>"
        , "<html>"
          , "<head>"
            , "<meta charset='utf-8'>"
            , "<title>", escapeString (episodeTitle episode), " :: Haskell Weekly Podcast</title>"
          , "</head>"
          , "<body>"
            , "<h1>Haskell Weekly Podcast</h1>"
            , "<h2>", escapeString (episodeTitle episode), "</h2>"
            , "<p>", escapeString (formatDescription (episodeDescription episode)), "</p>"
            , "<audio controls src='", escapeString (formatUri (episodeUrl episode)) ,"'></audio"
          , "</body>"
        , "</html>"
        ]))
    episodes

  -- https://help.apple.com/itc/podcasts_connect/#/itcbaf351599
  writeFile (FilePath.combine output "feed.rss") (concat
    [ "<?xml version='1.0' encoding='utf-8'?>"
    , "<rss version='2.0' xmlns:itunes='http://www.itunes.com/dtds/podcast-1.0.dtd'>"
      , "<channel>"
        , "<title>Haskell Weekly</title>"
        , "<link>", escapeString (formatUri root), "</link>"
        , "<description>"
          , "Short, casual discussion about the Haskell programming language."
        , "</description>"
        , "<itunes:author>Taylor Fausak</itunes:author>"
        , "<language>en-US</language>"
        , "<itunes:category text='Technology' />"
        , "<image>"
          , "<title>Haskell Weekly</title>"
          , "<link>", escapeString (formatUri root), "</link>"
          , "<url>", escapeString (formatUri root), "/logo.png</url>"
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
        , "<p><a href='feed.rss'>RSS feed</a></p>"
        , "<ul>"
          , concatMap
            (\ episode -> concat
              [ "<li>"
                , "<a href='", escapeString (episodeLink root episode), "'>"
                  , escapeString (episodeTitle episode)
                , "</a>"
              , "</li>"
              ])
            episodes
        , "</ul>"
      , "</body>"
    , "</html>"
    ])

formatEpisode :: Uri.URI -> Episode -> String
formatEpisode root episode = concat
  [ "<item>"
    , "<title>", escapeString (episodeTitle episode), "</title>"
    , "<link>", escapeString (episodeLink root episode), "</link>"
    , "<guid isPermalink='false'>", escapeString (formatUuid (episodeGuid episode)), "</guid>"
    , "<description>", escapeString (formatDescription (episodeDescription episode)), "</description>"
    , "<itunes:author>Taylor Fausak</itunes:author>"
    , "<enclosure "
      , "type='audio/mpeg' "
      , "length='", escapeString (formatBytes (episodeSize episode)), "' "
      , "url='", escapeString (formatUri (episodeUrl episode)), "' />"
    , "<itunes:duration>"
      , escapeString (formatSeconds (episodeDuration episode))
    , "</itunes:duration>"
    , "<pubDate>", escapeString (formatTime (episodeTime episode)), "</pubDate>"
  , "</item>"
  ]

episodeDefinitions :: [Either String Episode]
episodeDefinitions =
  [ Episode
    <$> Number.fromNatural 2
    <*> pure (Description.fromString "Sara Lichtenstein talks about upgrading Elm.")
    <*> parseUri "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
    <*> pure (Bytes.fromNatural 21580339)
    <*> pure (Seconds.fromNatural 1019)
    <*> parseUuid "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> toUTCTime 2019 3 13 12 0 0
  , Episode
    <$> Number.fromNatural 1
    <*> pure (Description.fromString "Cody Goodman talks about exceptions.")
    <*> parseUri "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
    <*> pure (Bytes.fromNatural 13999481)
    <*> pure (Seconds.fromNatural 583)
    <*> parseUuid "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> toUTCTime 2019 3 6 12 0 0
  ]

data Episode = Episode
  { episodeNumber :: Number.Number
  , episodeDescription :: Description.Description
  , episodeUrl :: Uri.URI
  , episodeSize :: Bytes.Bytes
  , episodeDuration :: Seconds.Seconds
  , episodeGuid :: Uuid.UUID
  , episodeTime :: Time.UTCTime
  } deriving (Eq, Show)

episodeLink :: Uri.URI -> Episode -> String
episodeLink root episode = concat
  [formatUri root, "/episodes/", formatNumber (episodeNumber episode), ".html"]

formatBytes :: Bytes.Bytes -> String
formatBytes = show . Bytes.toNatural

formatSeconds :: Seconds.Seconds -> String
formatSeconds seconds =
  let (m, s) = quotRem (Seconds.toNatural seconds) 60
  in Printf.printf "%d:%02d" m s

formatNumber :: Number.Number -> String
formatNumber = show . Number.toNatural

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

parseUri :: String -> Either String Uri.URI
parseUri string = case Uri.parseURIReference string of
  Nothing -> Left ("invalid URI: " ++ show string)
  Just uri -> Right uri

formatUri :: Uri.URI -> String
formatUri uri = Uri.uriToString id uri ""

parseUuid :: String -> Either String Uuid.UUID
parseUuid string = case Uuid.fromString string of
  Nothing -> Left ("invalid UUID: " ++ show string)
  Just uuid -> Right uuid

formatUuid :: Uuid.UUID -> String
formatUuid = Uuid.toString

formatDescription :: Description.Description -> String
formatDescription = Description.toString

toUTCTime :: Integer -> Int -> Int -> Int -> Int -> Fixed.Pico -> Either String Time.UTCTime
toUTCTime year month day hour minute second = do
  date <- toDay year month day
  time <- toTimeOfDay hour minute second
  Right (Time.localTimeToUTC Time.utc (Time.LocalTime date time))

toDay :: Integer -> Int -> Int -> Either String Time.Day
toDay y m d = case Time.fromGregorianValid y m d of
  Nothing -> Left ("invalid Day: " ++ show (y, m, d))
  Just day -> Right day

toTimeOfDay :: Int -> Int -> Fixed.Pico -> Either String Time.TimeOfDay
toTimeOfDay h m s = case Time.makeTimeOfDayValid h m s of
  Nothing -> Left ("invalid TimeOfDay: " ++ show (h, m, s))
  Just timeOfDay -> Right timeOfDay

formatTime :: Time.UTCTime -> String
formatTime = Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat

episodeTitle :: Episode -> String
episodeTitle episode = "Episode " ++ formatNumber (episodeNumber episode)
