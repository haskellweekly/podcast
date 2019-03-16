module Podcast.Main
  ( defaultMain
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
import qualified Podcast.Xml as Xml
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
        (FilePath.addExtension (show (Number.toNatural (Episode.number episode))) "html"))
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
            , "<p>", escapeString (Description.toString (Episode.description episode)), "</p>"
            , "<audio controls src='", escapeString (Url.toString (Episode.url episode)) ,"'></audio"
          , "</body>"
        , "</html>"
        ]))
    episodes

  -- https://help.apple.com/itc/podcasts_connect/#/itcbaf351599
  writeFile (FilePath.combine output "feed.rss") (Xml.render (Xml.root "rss"
    [ ("version", "2.0")
    , ("xmlns:itunes", "http://www.itunes.com/dtds/podcast-1.0.dtd")
    ]
    [ Xml.node "channel" []
      ( Xml.node "title" [] [Xml.text "Haskell Weekly"]
      : Xml.node "link" [] [Xml.text (Url.toString root)]
      : Xml.node "description" [] [Xml.text "Short, casual discussion about the Haskell programming language."]
      : Xml.node "itunes:author" [] [Xml.text "Taylor Fausak"]
      : Xml.node "language" [] [Xml.text "en-US"]
      : Xml.node "itunes:category" [("text", "Technology")] []
      : Xml.node "image" []
        [ Xml.node "title" [] [Xml.text "Haskell Weekly"]
        , Xml.node "link" [] [Xml.text (Url.toString root)]
        , Xml.node "url" [] [Xml.text (Url.toString root <> "/logo.png")]
        ]
      : map (episodeToRssItem root) episodes)
    ]))

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

episodeToRssItem :: Url.Url -> Episode.Episode -> Xml.Node
episodeToRssItem root episode = Xml.node "item" []
  [ Xml.node "title" [] [Xml.text (episodeTitle episode)]
  , Xml.node "link" [] [Xml.text (episodeLink root episode)]
  , Xml.node "guid" [("isPermalink", "false")] [Xml.text (Guid.toString (Episode.guid episode))]
  , Xml.node "description" [] [Xml.text (Description.toString (Episode.description episode))]
  , Xml.node "itunes:author" [] [Xml.text "Haskell Weekly"]
  , Xml.node "enclosure"
    [ ("type", "audio/mpeg")
    , ("length", show (Bytes.toNatural (Episode.size episode)))
    , ("url", Url.toString (Episode.url episode))
    ]
    []
  , Xml.node "itunes:duration" [] [Xml.text (formatSeconds (Episode.duration episode))]
  , Xml.node "pubDate" [] [Xml.text (Time.toString (Episode.time episode))]
  ]

episodeDefinitions :: [Either String Episode.Episode]
episodeDefinitions =
  [ Episode.Episode
    <$> Description.fromString "Sara Lichtenstein talks about upgrading Elm."
    <*> pure (Seconds.fromNatural 899)
    <*> Guid.fromString "00900298-5aa6-4301-a207-619d38cdc81a"
    <*> Number.fromNatural 2
    <*> pure (Bytes.fromNatural 21580339)
    <*> Time.fromString "2019-03-13T12:00:00"
    <*> Url.fromString "https://user.fm/files/v2-713fb5701a33ecfce9fbd9d407df747f/episode-2.mp3"
  , Episode.Episode
    <$> Description.fromString "Cody Goodman talks about exceptions."
    <*> pure (Seconds.fromNatural 583)
    <*> Guid.fromString "6fe12dba-e0c3-4af5-b9fc-844bc2396ae7"
    <*> Number.fromNatural 1
    <*> pure (Bytes.fromNatural 13999481)
    <*> Time.fromString "2019-03-06T12:00:00"
    <*> Url.fromString "https://user.fm/files/v2-9466bdde6ba1f30d51e417712da15053/episode-1.mp3"
  ]

episodeLink :: Url.Url -> Episode.Episode -> String
episodeLink root episode = concat
  [Url.toString root, "/episodes/", show (Number.toNatural (Episode.number episode)), ".html"]

formatSeconds :: Seconds.Seconds -> String
formatSeconds seconds =
  let (m, s) = quotRem (Seconds.toNatural seconds) 60
  in Printf.printf "%d:%02d" m s

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

episodeTitle :: Episode.Episode -> String
episodeTitle episode = "Episode " ++ show (Number.toNatural (Episode.number episode))
