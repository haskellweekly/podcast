module Podcast.Type.Seconds
  ( Seconds
  , fromNatural
  , fromTimestamp
  , toNatural
  , toString
  )
where

import qualified Numeric.Natural as Natural
import qualified Text.Printf as Printf

newtype Seconds
  = Seconds Natural.Natural
  deriving (Eq, Ord, Show)

fromNatural :: Natural.Natural -> Seconds
fromNatural = Seconds

fromTimestamp :: Natural.Natural -> Natural.Natural -> Either String Seconds
fromTimestamp minutes seconds = if seconds >= 60
  then Left ("invalid Seconds: " <> show (minutes, seconds))
  else Right (fromNatural ((60 * minutes) + seconds))

toNatural :: Seconds -> Natural.Natural
toNatural (Seconds natural) = natural

toString :: Seconds -> String
toString seconds =
  let (m, s) = quotRem (toNatural seconds) 60 in Printf.printf "%d:%02d" m s
