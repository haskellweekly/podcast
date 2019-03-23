module Podcast.Type.Url
  ( Url
  , fromString
  , fromUri
  , toString
  , toUri
  , combine
  )
where

import qualified Network.URI as Uri

newtype Url
  = Url Uri.URI
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Url
fromString string = case Uri.parseURIReference string of
  Nothing -> Left ("invalid Url: " ++ show string)
  Just uri -> Right (fromUri uri)

fromUri :: Uri.URI -> Url
fromUri = Url

toString :: Url -> String
toString url = Uri.uriToString id (toUri url) ""

toUri :: Url -> Uri.URI
toUri (Url uri) = uri

combine :: Url -> Url -> Url
combine root path =
  fromUri (Uri.nonStrictRelativeTo (toUri path) (toUri root))
