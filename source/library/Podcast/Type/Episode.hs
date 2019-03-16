module Podcast.Type.Episode
  ( Episode(..)
  )
where

import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Time as Time
import qualified Podcast.Type.Url as Url

data Episode = Episode
  { description :: Description.Description
  , duration :: Seconds.Seconds
  , guid :: Guid.Guid
  , number :: Number.Number
  , size :: Bytes.Bytes
  , time :: Time.Time
  , url :: Url.Url
  } deriving (Eq, Ord, Show)
