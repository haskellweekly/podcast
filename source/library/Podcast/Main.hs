module Podcast.Main
  ( defaultMain
  )
where

import qualified Podcast.Episodes as Episodes
import qualified Podcast.Html as Html
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
  episodes <- either fail pure (sequence Episodes.episodes)
  let
    input = "input"
    output = "output"
    directory = FilePath.combine output "episodes"
  Directory.createDirectoryIfMissing True output
  Directory.createDirectoryIfMissing True directory

  Directory.copyFile
    (FilePath.combine input "logo.png")
    (FilePath.combine output "logo.png")

  mapM_
    (\ episode -> writeFile
      (episodePath directory episode)
      (episodeToHtml episode))
    episodes

  -- https://help.apple.com/itc/podcasts_connect/#/itcbaf351599
  writeFile
    (FilePath.combine output "feed.rss")
    (Xml.render (episodesToRss root episodes))

  writeFile (FilePath.combine output "index.html") (index episodes)

episodePath :: FilePath -> Episode.Episode -> FilePath
episodePath directory episode =
  FilePath.combine
    directory
    (FilePath.addExtension
      (show (Number.toNatural (Episode.number episode)))
      "html")

episodeToHtml :: Episode.Episode -> String
episodeToHtml episode = Html.render (Html.element "html" []
  [ Html.node "head" []
    [ Html.node "meta" [("charset", "utf-8")] []
    , Html.node "title" [] [Html.text (episodeTitle episode ++ " :: Haskell Weekly Podcast")]
    ]
  , Html.node "body" []
    [ Html.node "h1" [] [Html.text "Haskell Weekly Podcast"]
    , Html.node "h2" [] [Html.text (episodeTitle episode)]
    , Html.node "p" [] [Html.text (Description.toString (Episode.description episode))]
    , Html.node "audio"
      [("controls", ""), ("src", Url.toString (Episode.url episode))]
      [Html.text ""]
    ]
  ])

episodesToRss :: Url.Url -> [Episode.Episode] -> Xml.Element
episodesToRss root episodes = Xml.element "rss"
  [ ("version", "2.0")
  , ("xmlns:itunes", "http://www.itunes.com/dtds/podcast-1.0.dtd")
  ]
  [ Xml.node "channel" []
    ( Xml.node "title" [] [Xml.text "Haskell Weekly"]
    : Xml.node "link" [] [Xml.text (Url.toString root)]
    : Xml.node "description" [] [Xml.text "Short, casual discussion about the Haskell programming language."]
    : Xml.node "itunes:author" [] [Xml.text "Haskell Weekly"]
    : Xml.node "language" [] [Xml.text "en-US"]
    : Xml.node "itunes:category" [("text", "Technology")] []
    : Xml.node "image" []
      [ Xml.node "title" [] [Xml.text "Haskell Weekly"]
      , Xml.node "link" [] [Xml.text (Url.toString root)]
      , Xml.node "url" [] [Xml.text (Url.toString root ++ "/logo.png")]
      ]
    : map (episodeToRssItem root) episodes)
  ]

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

index :: [Episode.Episode] -> String
index episodes = Html.render (Html.element "html" []
  [ Html.node "head" []
    [ Html.node "meta" [("charset", "utf-8")] []
    , Html.node "title" [] [Html.text "Haskell Weekly Podcast"]
    ]
  , Html.node "body" []
    [ Html.node "h1" [] [Html.text "Haskell Weekly Podcast"]
    , Html.node "p" []
      [Html.node "a" [("href", "feed.rss")] [Html.text "RSS feed"]]
    , Html.node "ul" [] (map
      (\ episode -> Html.node "li" []
        [ Html.node "a"
          [("href", episodePath "episodes" episode)]
          [Html.text (episodeTitle episode)]
        ])
      episodes)
    ]
  ])

episodeLink :: Url.Url -> Episode.Episode -> String
episodeLink root episode = concat
  [ Url.toString root
  , "/episodes/"
  , show (Number.toNatural (Episode.number episode))
  , ".html"
  ]

formatSeconds :: Seconds.Seconds -> String
formatSeconds seconds =
  let (m, s) = quotRem (Seconds.toNatural seconds) 60
  in Printf.printf "%d:%02d" m s

episodeTitle :: Episode.Episode -> String
episodeTitle episode =
  "Episode " ++ show (Number.toNatural (Episode.number episode))