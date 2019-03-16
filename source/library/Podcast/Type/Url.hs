module Podcast.Type.Url
  ( Url
  , fromString
  , toString
  )
where

import qualified Network.URI as Uri

newtype Url
  = Url Uri.URI
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Url
fromString string = case Uri.parseURIReference string of
  Nothing -> Left ("invalid Url: " <> show string)
  Just uri -> Right (Url uri)

toString :: Url -> String
toString (Url uri) = Uri.uriToString id uri ""
