module Podcast.Type.Articles
  ( Articles
  , fromStrings
  , toStrings
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Podcast.Type.Article as Article

type Articles = NonEmpty.NonEmpty Article.Article

fromStrings :: [String] -> Either String Articles
fromStrings list = case NonEmpty.nonEmpty list of
  Nothing -> Left ("invalid Articles: " <> show list)
  Just nonEmpty -> traverse Article.fromString nonEmpty

toStrings :: Articles -> [String]
toStrings = fmap Article.toString . NonEmpty.toList
