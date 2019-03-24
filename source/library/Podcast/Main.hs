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
import qualified Podcast.Site.Index as Index
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
  (Route.toFilePath Route.Index, pure (toUtf8 (Html.render (Index.html root episodes))))
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

episodeTitle :: Episode.Episode -> String
episodeTitle episode =
  "Episode " ++ Number.toString (Episode.number episode)
