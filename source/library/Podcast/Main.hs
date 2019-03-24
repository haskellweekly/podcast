module Podcast.Main
  ( defaultMain
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Podcast.Episodes as Episodes
import qualified Podcast.Html as Html
import qualified Podcast.Site.Feed as Feed
import qualified Podcast.Site.Logo as Logo
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Time as Time
import qualified Podcast.Type.Url as Url
import qualified Podcast.Xml as Xml
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

defaultMain :: IO ()
defaultMain = do
  let output = "output"
  removeDirectory output

  root <- getRootUrl
  episodes <- getEpisodes
  let site = makeSite root episodes

  createDirectories output site
  createFiles output site

createDirectories :: FilePath -> [(FilePath, a)] -> IO ()
createDirectories directory site = Foldable.traverse_
  createDirectory
  (Set.fromList (map (getDirectory directory) site))

getDirectory :: FilePath -> (FilePath, a) -> FilePath
getDirectory directory (file, _) =
  FilePath.takeDirectory (FilePath.combine directory file)

createFiles :: FilePath -> [(FilePath, IO ByteString.ByteString)] -> IO ()
createFiles directory site = Foldable.traverse_
  (createFile directory)
  site

createFile :: FilePath -> (FilePath, IO ByteString.ByteString) -> IO ()
createFile directory (file, generate) = do
  contents <- generate
  ByteString.writeFile (FilePath.combine directory file) contents

removeDirectory :: FilePath -> IO ()
removeDirectory = Directory.removePathForcibly

createDirectory :: FilePath -> IO ()
createDirectory = Directory.createDirectoryIfMissing True

getRootUrl :: IO Url.Url
getRootUrl = do
  maybeString <- Environment.lookupEnv "ROOT_URL"
  fromRight (Url.fromString (Maybe.fromMaybe "./" maybeString))

getEpisodes :: IO [Episode.Episode]
getEpisodes = fromRight (sequence Episodes.episodes)

fromRight :: Either String a -> IO a
fromRight = either fail pure

makeSite :: Url.Url -> [Episode.Episode] -> [(FilePath, IO ByteString.ByteString)]
makeSite root episodes =
  (Route.toFilePath Route.Index, pure (toUtf8 (index root episodes)))
  : (Route.toFilePath Route.AppleBadge, ByteString.readFile "input/listen-on-apple-podcasts.svg")
  : (Route.toFilePath Route.GoogleBadge, ByteString.readFile "input/listen-on-google-podcasts.svg")
  : (Route.toFilePath Route.Logo, ByteString.readFile "input/logo.png")
  : (Route.toFilePath Route.Feed, pure (toUtf8 (Xml.render (Feed.rss root episodes))))
  : map
    (\ episode ->
      ( Route.toFilePath (Route.Episode (Episode.number episode))
      , pure (toUtf8 (episodeToHtml episode))
      ))
    episodes

toUtf8 :: String -> ByteString.ByteString
toUtf8 string = Encoding.encodeUtf8 (Text.pack string)

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
    , Html.node "div" [("style", "width:100px;height:100px;background:#5c3566")] [Logo.svg]
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
          [("href", episodeLink root episode)]
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
