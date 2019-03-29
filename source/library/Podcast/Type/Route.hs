module Podcast.Type.Route
  ( Route(..)
  , toFilePath
  , toUrl
  )
where

import qualified Network.URI as Uri
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Url as Url

data Route
  = AppleBadge
  | Bootstrap
  | Episode Number.Number
  | Feed
  | GoogleBadge
  | Index
  | Logo
  deriving (Eq, Ord, Show)

toFilePath :: Route -> FilePath
toFilePath route = case route of
  AppleBadge -> "apple-badge.svg"
  Bootstrap -> "bootstrap-3.4.1.svg"
  Episode number -> concat ["episodes/", Number.toString number, ".html"]
  Feed -> "feed.rss"
  GoogleBadge -> "google-badge.svg"
  Index -> "index.html"
  Logo -> "logo.png"

toUrl :: Route -> Url.Url
toUrl route = Url.fromUri Uri.nullURI
  { Uri.uriPath = Uri.escapeURIString Uri.isUnescapedInURI (toFilePath route)
  }
