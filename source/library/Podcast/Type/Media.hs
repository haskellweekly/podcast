module Podcast.Type.Media
  ( Media
  , fromString
  , toString
  )
where

import qualified Podcast.Type.Url as Url

newtype Media
  = Media Url.Url
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Media
fromString string = fmap Media (Url.fromString string)

toString :: Media -> String
toString (Media url) = Url.toString url
