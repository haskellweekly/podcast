module Podcast.Main
  ( defaultMain
  )
where

import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Time as Time
import qualified Podcast.Type.Url as Url
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Text.Printf as Printf

defaultMain :: IO ()
defaultMain = do
  root <- either fail pure (Url.fromString "https://haskellweekly.news/podcast")
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
            , "<audio controls src='", escapeString (Url.toString (episodeUrl episode)) ,"'></audio"
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
        , "<link>", escapeString (Url.toString root), "</link>"
        , "<description>"
          , "Short, casual discussion about the Haskell programming language."
        , "</description>"
        , "<itunes:author>Taylor Fausak</itunes:author>"
        , "<language>en-US</language>"
        , "<itunes:category text='Technology' />"
        , "<image>"
          , "<title>Haskell Weekly</title>"
          , "<link>", escapeString (Url.toString root), "</link>"
          , "<url>", escapeString (Url.toString root), "/logo.png</url>"
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

formatEpisode :: Url.Url -> Episode -> String
formatEpisode root episode = concat
  [ "<item>"
    , "<title>", escapeString (episodeTitle episode), "</title>"
    , "<link>", escapeString (episodeLink root episode), "</link>"
    , "<guid isPermalink='false'>", escapeString (Guid.toString (episodeGuid episode)), "</guid>"
    , "<description>", escapeString (formatDescription (episodeDescription episode)), "</description>"
    , "<itunes:author>Taylor Fausak</itunes:author>"
    , "<enclosure "
      , "type='audio/mpeg' "
      , "length='", escapeString (formatBytes (episodeSize episode)), "' "
      , "url='", escapeString (Url.toString (episodeUrl episode)), "' />"
    , "<itunes:duration>"
      , escapeString (formatSeconds (episodeDuration episode))
    , "</itunes:duration>"
    , "<pubDate>", escapeString (Time.toString (episodeTime episode)), "</pubDate>"
  , "</item>"
  ]

episodeDefinitions :: [Either String Episode]
episodeDefinitions =
  [ Episode
    <$> Number.fromNatural 2
    <*> Description.fromString "Sara Lichtenstein talks about upgrading Elm."
    <*> Url.fromString "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
    <*> pure (Bytes.fromNatural 21580339)
    <*> pure (Seconds.fromNatural 1019)
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Time.fromString "2019-03-13T12:00:00"
  , Episode
    <$> Number.fromNatural 1
    <*> Description.fromString "Cody Goodman talks about exceptions."
    <*> Url.fromString "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
    <*> pure (Bytes.fromNatural 13999481)
    <*> pure (Seconds.fromNatural 583)
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Time.fromString "2019-03-06T12:00:00"
  ]

data Episode = Episode
  { episodeNumber :: Number.Number
  , episodeDescription :: Description.Description
  , episodeUrl :: Url.Url
  , episodeSize :: Bytes.Bytes
  , episodeDuration :: Seconds.Seconds
  , episodeGuid :: Guid.Guid
  , episodeTime :: Time.Time
  } deriving (Eq, Show)

episodeLink :: Url.Url -> Episode -> String
episodeLink root episode = concat
  [Url.toString root, "/episodes/", formatNumber (episodeNumber episode), ".html"]

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

formatDescription :: Description.Description -> String
formatDescription = Description.toString

episodeTitle :: Episode -> String
episodeTitle episode = "Episode " ++ formatNumber (episodeNumber episode)
