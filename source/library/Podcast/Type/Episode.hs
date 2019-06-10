module Podcast.Type.Episode
  ( Episode(..)
  )
where

import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Media as Media

data Episode = Episode
  { date :: Date.Date
  , description :: Description.Description
  , duration :: Seconds.Seconds
  , guid :: Guid.Guid
  , number :: Number.Number
  , size :: Bytes.Bytes
  , title :: Title.Title
  , url :: Media.Media
  } deriving (Eq, Ord, Show)
