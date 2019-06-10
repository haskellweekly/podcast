module Podcast.Type.Article
  ( Article
  , fromString
  , toString
  )
where

import qualified Podcast.Type.Url as Url

newtype Article
  = Article Url.Url
  deriving (Eq, Ord, Show)

fromString :: String -> Either String Article
fromString string = fmap Article (Url.fromString string)

toString :: Article -> String
toString (Article url) = Url.toString url
