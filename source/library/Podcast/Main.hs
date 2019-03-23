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
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Time as Time
import qualified Podcast.Type.Url as Url
import qualified Podcast.Xml as Xml
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  root <- either fail pure (Url.fromString "https://haskellweekly.news/podcast")
  episodes <- either fail pure (sequence Episodes.episodes)
  let
    input = "input"
    output = "output"
  Directory.createDirectoryIfMissing True output
  Directory.createDirectoryIfMissing True (FilePath.combine output "episodes")

  mapM_
    (\ file -> Directory.copyFile
      (FilePath.combine input file)
      (FilePath.combine output file))
    [ Route.toFilePath Route.AppleBadge
    , Route.toFilePath Route.GoogleBadge
    , Route.toFilePath Route.Logo
    ]

  mapM_
    (\ episode -> writeFileUTF8
      (episodePath episode)
      (episodeToHtml episode))
    episodes

  -- https://help.apple.com/itc/podcasts_connect/#/itcbaf351599
  writeFileUTF8
    (FilePath.combine output (Route.toFilePath Route.Feed))
    (Xml.render (episodesToRss root episodes))

  writeFileUTF8 (FilePath.combine output (Route.toFilePath Route.Index)) (index root episodes)

episodePath :: Episode.Episode -> FilePath
episodePath episode = Route.toFilePath (Route.Episode (Episode.number episode))

episodeToHtml :: Episode.Episode -> String
episodeToHtml episode = Html.render (Html.element "html" []
  [ Html.node "head" []
    [ Html.node "meta" [("charset", "utf-8")] []
    , Html.node "title" [] [Html.text (episodeTitle episode ++ " :: Haskell Weekly podcast")]
    ]
  , Html.node "body" []
    [ Html.node "h1" [] [Html.text "Haskell Weekly podcast"]
    , Html.node "h2" [] [Html.text (episodeTitle episode)]
    , Html.node "p" [] [Html.text (Description.toString (Episode.description episode))]
    , Html.node "audio"
      [("controls", ""), ("src", Url.toString (Episode.url episode))]
      [Html.text ""]
    , Html.node "p" []
      [ Html.text "Published on "
      , Html.text (Time.toDateString (Episode.time episode))
      , Html.text "."
      ]
    ]
  ])

episodesToRss :: Url.Url -> [Episode.Episode] -> Xml.Element
episodesToRss root episodes = Xml.element "rss"
  [ ("version", "2.0")
  , ("xmlns:atom", "http://www.w3.org/2005/Atom")
  , ("xmlns:itunes", "http://www.itunes.com/dtds/podcast-1.0.dtd")
  ]
  [ Xml.node "channel" []
    ( Xml.node "title" [] [Xml.text "Haskell Weekly"]
    : Xml.node "link" [] [Xml.text (Url.toString root)]
    : Xml.node "description" [] [Xml.text podcastDescription]
    : Xml.node "itunes:author" [] [Xml.text "Taylor Fausak"]
    : Xml.node "language" [] [Xml.text "en-US"]
    : Xml.node "itunes:explicit" [] [Xml.text "clean"]
    : Xml.node "copyright" [] [Xml.text "\xa9 2019 Taylor Fausak"]
    : Xml.node "itunes:category" [("text", "Technology")] []
    : Xml.node "itunes:owner" []
      [ Xml.node "itunes:name" [] [Xml.text "Taylor Fausak"]
      , Xml.node "itunes:email" [] [Xml.text "taylor@fausak.me"]
      ]
    : Xml.node "atom:link"
      [ ("rel", "self")
      , ("href", Url.toString (Url.combine root (Route.toUrl Route.Feed)))
      , ("type", "application/rss+xml")
      ]
      []
    : Xml.node "image" []
      [ Xml.node "title" [] [Xml.text "Haskell Weekly"]
      , Xml.node "link" [] [Xml.text (Url.toString root)]
      , Xml.node "url" [] [Xml.text (Url.toString (Url.combine root (Route.toUrl Route.Logo)))]
      ]
    : map (episodeToRssItem root) episodes)
  ]

episodeToRssItem :: Url.Url -> Episode.Episode -> Xml.Node
episodeToRssItem root episode = Xml.node "item" []
  [ Xml.node "title" [] [Xml.text (episodeTitle episode)]
  , Xml.node "link" [] [Xml.text (episodeLink root episode)]
  , Xml.node "guid" [("isPermaLink", "false")] [Xml.text (Guid.toString (Episode.guid episode))]
  , Xml.node "description" [] [Xml.text (Description.toString (Episode.description episode))]
  , Xml.node "itunes:author" [] [Xml.text "Taylor Fausak"]
  , Xml.node "enclosure"
    [ ("type", "audio/mpeg")
    , ("length", Bytes.toString (Episode.size episode))
    , ("url", Url.toString (Episode.url episode))
    ]
    []
  , Xml.node "itunes:duration" [] [Xml.text (Seconds.toString (Episode.duration episode))]
  , Xml.node "pubDate" [] [Xml.text (Time.toRfc822 (Episode.time episode))]
  ]

index :: Url.Url -> [Episode.Episode] -> String
index root episodes = Html.render (Html.element "html" []
  [ Html.node "head" []
    [ Html.node "meta" [("charset", "utf-8")] []
    , Html.node "meta"
      [ ("name", "viewport")
      , ("content", "initial-scale = 1, width = device-width")
      ] []
    , Html.node "title" [] [Html.text "Haskell Weekly podcast"]
    , Html.node "link"
      [ ("href", Url.toString (Url.combine root (Route.toUrl Route.Feed)))
      , ("rel", "alternate")
      , ("title", "Haskell Weekly podcast")
      , ("type", "application/rss+xml")
      ]
      []
    ]
  , Html.node "body" []
    [ Html.node "h1" [] [Html.text "Haskell Weekly podcast"]
    , Html.node "div" [("style", "width:100px;height:100px;background:#5c3566")] [logoSvg]
    , Html.node "p" [] [Html.text podcastDescription]
    , Html.node "ul" []
      [ Html.node "li" []
        [ Html.node "a"
          [ ("href", "https://itunes.apple.com/us/podcast/haskell-weekly/id1456545040?mt=2&app=podcast")
          ]
          [ Html.node "img"
            [ ("alt", "Listen on Apple Podcasts")
            , ("src", Url.toString (Url.combine root (Route.toUrl Route.AppleBadge)))
            , ("width", "200")
            , ("height", "49")
            ] []
          ]
        ]
      , Html.node "li" []
        [ Html.node "a"
          [ ("href", "https://playmusic.app.goo.gl/?ibi=com.google.PlayMusic&isi=691797987&ius=googleplaymusic&apn=com.google.android.music&link=https://play.google.com/music/m/Irjo4hxyfeiid3zhasycmgs3o2q?t%3DHaskell_Weekly%26pcampaignid%3DMKT-na-all-co-pr-mu-pod-16")
          ]
          [ Html.node "img"
            [ ("alt", "Listen on Google Podcasts")
            , ("src", Url.toString (Url.combine root (Route.toUrl Route.GoogleBadge)))
            , ("width", "200")
            , ("height", "51")
            ] []
          ]
        ]
      , Html.node "li" []
        [ Html.node "a" [("href", Route.toFilePath Route.Feed)] [Html.text "RSS feed"]
        ]
      ]
    , Html.node "h2" [] [Html.text "Episodes"]
    , Html.node "ul" [] (map
      (\ episode -> Html.node "li" []
        [ Html.text (Time.toDateString (Episode.time episode))
        , Html.text " "
        , Html.node "a"
          [("href", episodePath episode)]
          [Html.text (episodeTitle episode)]
        ])
      episodes)
    ]
  ])

episodeLink :: Url.Url -> Episode.Episode -> String
episodeLink root episode = Url.toString (Url.combine root (Route.toUrl (Route.Episode (Episode.number episode))))

episodeTitle :: Episode.Episode -> String
episodeTitle episode =
  "Episode " ++ Number.toString (Episode.number episode)

podcastDescription :: String
podcastDescription =
  "Haskell Weekly covers the Haskell progamming langauge. Listen to \
  \professional software developers discuss using functional programming to \
  \solve real-world business problems. Each episode uses a conversational two-\
  \host format and runs for about 15 minutes."

writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 file contents = IO.withFile file IO.WriteMode (\ handle -> do
  IO.hSetEncoding handle IO.utf8
  IO.hPutStr handle contents)

logoSvg :: Xml.Node
logoSvg = Xml.node "svg"
  [ ("xmlns", "http://www.w3.org/2000/svg")
  , ("viewBox", "0 0 100 100")
  ]
  [ Xml.node "path"
    [ ("fill", "#fff")
    , ("opacity", "0.8")
    , ("d", "M 80.8 49.9 l -19 28.4 H 76 l 19 -28.4 z m -61.6 0 l 19 28.4 H 24 L 5 49.9 z")
    ] []
  , Xml.node "path"
    [ ("fill", "#fff")
    , ("d", "M 24 78.3 l 19 -28.4 -19 -28.4 h 14.2 l 38 56.8 H 61.8 L 50 60.5 l -11.8 18 H 24 z m 0 0")
    ] []
  ]
