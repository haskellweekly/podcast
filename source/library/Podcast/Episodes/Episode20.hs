module Podcast.Episodes.Episode20
  ( episode20
  )
where

import qualified Podcast.Type.Articles as Articles
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Episode as Episode
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title

episode20 :: Either String Episode.Episode
episode20 =
  Episode.Episode
    <$> Articles.fromStrings
          ["https://typeclasses.com/featured/rounding"]
    <*> Date.fromGregorian 2019 9 13
    <*> Description.fromString
          "Sara Lichtenstein and Taylor Fausak discuss converting between numeric types with polymorphic functions."
    <*> Seconds.fromTimestamp 15 49
    <*> Guid.fromString "d916811b-886b-4da7-a104-ff65bda7124c"
    <*> Media.fromString
          "https://haskell-weekly-podcast.nyc3.cdn.digitaloceanspaces.com/2019-09-13-episode-20.mp3"
    <*> Number.fromNatural 20
    <*> Right (Bytes.fromNatural 22811159)
    <*> Title.fromString "Polymorphic Rounding"
    <*> Right Nothing
