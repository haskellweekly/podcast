module Podcast.Episodes
  ( episodes
  )
where

import qualified Podcast.Episodes.Episode1 as Episodes
import qualified Podcast.Episodes.Episode2 as Episodes
import qualified Podcast.Episodes.Episode3 as Episodes
import qualified Podcast.Episodes.Episode4 as Episodes
import qualified Podcast.Episodes.Episode5 as Episodes
import qualified Podcast.Episodes.Episode6 as Episodes
import qualified Podcast.Episodes.Episode7 as Episodes
import qualified Podcast.Episodes.Episode8 as Episodes
import qualified Podcast.Episodes.Episode9 as Episodes
import qualified Podcast.Episodes.Episode10 as Episodes
import qualified Podcast.Episodes.Episode11 as Episodes
import qualified Podcast.Episodes.Episode12 as Episodes
import qualified Podcast.Episodes.Episode13 as Episodes
import qualified Podcast.Episodes.Episode14 as Episodes
import qualified Podcast.Episodes.Episode15 as Episodes
import qualified Podcast.Episodes.Episode16 as Episodes
import qualified Podcast.Episodes.Episode17 as Episodes
import qualified Podcast.Episodes.Episode18 as Episodes
import qualified Podcast.Episodes.Episode19 as Episodes
import qualified Podcast.Episodes.Episode20 as Episodes
import qualified Podcast.Type.Episode as Episode

episodes :: [Either String Episode.Episode]
episodes =
  [ Episodes.episode20
  , Episodes.episode19
  , Episodes.episode18
  , Episodes.episode17
  , Episodes.episode16
  , Episodes.episode15
  , Episodes.episode14
  , Episodes.episode13
  , Episodes.episode12
  , Episodes.episode11
  , Episodes.episode10
  , Episodes.episode9
  , Episodes.episode8
  , Episodes.episode7
  , Episodes.episode6
  , Episodes.episode5
  , Episodes.episode4
  , Episodes.episode3
  , Episodes.episode2
  , Episodes.episode1
  ]
