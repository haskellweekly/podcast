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
import qualified Podcast.Site.Episode as Site.Episode
import qualified Podcast.Site.Feed as Feed
import qualified Podcast.Site.Index as Index
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Route as Route
import qualified Podcast.Type.Source as Source
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

createDirectories :: FilePath -> [(Route.Route, source)] -> IO ()
createDirectories directory site = Foldable.traverse_
  createDirectory
  (Set.fromList (map (getDirectory directory) site))

getDirectory :: FilePath -> (Route.Route, source) -> FilePath
getDirectory directory (route, _) =
  FilePath.takeDirectory (buildRoute directory route)

createFiles :: FilePath -> [(Route.Route, Source.Source)] -> IO ()
createFiles directory = Foldable.traverse_ (createFile directory)

createFile :: FilePath -> (Route.Route, Source.Source) -> IO ()
createFile directory (route, source) = do
  contents <- case source of
    Source.File file -> ByteString.readFile (FilePath.combine "input" file)
    Source.Html html -> pure (toUtf8 (Html.render html))
    Source.Xml xml -> pure (toUtf8 (Xml.render xml))
  ByteString.writeFile (buildRoute directory route) contents

buildRoute :: FilePath -> Route.Route -> FilePath
buildRoute directory route =
  FilePath.combine directory (Route.toFilePath route)

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

makeSite :: Url.Url -> [Episode.Episode] -> [(Route.Route, Source.Source)]
makeSite root episodes =
  makeAppleBadge
    : makeBootstrap
    : makeFeed root episodes
    : makeGoogleBadge
    : makeIndex root episodes
    : makeLogo
    : makeEpisodes root episodes

toUtf8 :: String -> ByteString.ByteString
toUtf8 string = Encoding.encodeUtf8 (Text.pack string)

makeAppleBadge :: (Route.Route, Source.Source)
makeAppleBadge = (Route.AppleBadge, Source.File "apple-badge.svg")

makeBootstrap :: (Route.Route, Source.Source)
makeBootstrap = (Route.Bootstrap, Source.File "bootstrap-4.3.1.css")

makeFeed :: Url.Url -> [Episode.Episode] -> (Route.Route, Source.Source)
makeFeed root episodes = (Route.Feed, Source.Xml (Feed.rss root episodes))

makeGoogleBadge :: (Route.Route, Source.Source)
makeGoogleBadge = (Route.GoogleBadge, Source.File "google-badge.svg")

makeIndex :: Url.Url -> [Episode.Episode] -> (Route.Route, Source.Source)
makeIndex root episodes =
  (Route.Index, Source.Html (Index.html root episodes))

makeLogo :: (Route.Route, Source.Source)
makeLogo = (Route.Logo, Source.File "logo.png")

makeEpisodes :: Url.Url -> [Episode.Episode] -> [(Route.Route, Source.Source)]
makeEpisodes root = map (makeEpisode root)

makeEpisode :: Url.Url -> Episode.Episode -> (Route.Route, Source.Source)
makeEpisode root episode =
  ( Route.Episode (Episode.number episode)
  , Source.Html (Site.Episode.html root episode)
  )
