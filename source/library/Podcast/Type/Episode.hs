module Podcast.Type.Episode
  ( Episode(..)
  )
where

import qualified Podcast.Type.Articles as Articles
import qualified Podcast.Type.Bytes as Bytes
import qualified Podcast.Type.Date as Date
import qualified Podcast.Type.Description as Description
import qualified Podcast.Type.Guid as Guid
import qualified Podcast.Type.Media as Media
import qualified Podcast.Type.Number as Number
import qualified Podcast.Type.Seconds as Seconds
import qualified Podcast.Type.Title as Title
import qualified Podcast.Type.Transcript as Transcript

data Episode = Episode
  { articles :: Articles.Articles
  , date :: Date.Date
  , description :: Description.Description
  , duration :: Seconds.Seconds
  , guid :: Guid.Guid
  , media :: Media.Media
  , number :: Number.Number
  , size :: Bytes.Bytes
  , title :: Title.Title
  , transcript :: Maybe Transcript.Transcript
  } deriving (Eq, Ord, Show)
