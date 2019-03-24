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

createDirectories :: FilePath -> [(Route.Route, a)] -> IO ()
createDirectories directory site = Foldable.traverse_
  createDirectory
  (Set.fromList (map (getDirectory directory) site))

getDirectory :: FilePath -> (Route.Route, a) -> FilePath
getDirectory directory (route, _) =
  FilePath.takeDirectory (buildRoute directory route)

createFiles :: FilePath -> [(Route.Route, IO ByteString.ByteString)] -> IO ()
createFiles directory site = Foldable.traverse_ (createFile directory) site

createFile :: FilePath -> (Route.Route, IO ByteString.ByteString) -> IO ()
createFile directory (route, generate) = do
  contents <- generate
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

makeSite
  :: Url.Url -> [Episode.Episode] -> [(Route.Route, IO ByteString.ByteString)]
makeSite root episodes =
  makeAppleBadge
    : makeFeed root episodes
    : makeGoogleBadge
    : makeIndex root episodes
    : makeLogo
    : makeEpisodes root episodes

toUtf8 :: String -> ByteString.ByteString
toUtf8 string = Encoding.encodeUtf8 (Text.pack string)

makeAppleBadge :: (Route.Route, IO ByteString.ByteString)
makeAppleBadge =
  (Route.AppleBadge, ByteString.readFile "input/listen-on-apple-podcasts.svg")

makeFeed
  :: Applicative f
  => Url.Url
  -> [Episode.Episode]
  -> (Route.Route, f ByteString.ByteString)
makeFeed root episodes =
  (Route.Feed, pure (toUtf8 (Xml.render (Feed.rss root episodes))))

makeGoogleBadge :: (Route.Route, IO ByteString.ByteString)
makeGoogleBadge =
  ( Route.GoogleBadge
  , ByteString.readFile "input/listen-on-google-podcasts.svg"
  )

makeIndex
  :: Applicative f
  => Url.Url
  -> [Episode.Episode]
  -> (Route.Route, f ByteString.ByteString)
makeIndex root episodes =
  (Route.Index, pure (toUtf8 (Html.render (Index.html root episodes))))

makeLogo :: (Route.Route, IO ByteString.ByteString)
makeLogo = (Route.Logo, ByteString.readFile "input/logo.png")

makeEpisodes
  :: Applicative f
  => Url.Url
  -> [Episode.Episode]
  -> [(Route.Route, f ByteString.ByteString)]
makeEpisodes root = map (makeEpisode root)

makeEpisode
  :: Applicative f
  => Url.Url
  -> Episode.Episode
  -> (Route.Route, f ByteString.ByteString)
makeEpisode root episode =
  ( Route.Episode (Episode.number episode)
  , pure (toUtf8 (Html.render (Site.Episode.html root episode)))
  )
