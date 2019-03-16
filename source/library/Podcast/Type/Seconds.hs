module Podcast.Type.Seconds
  ( Seconds
  , fromNatural
  , toNatural
  )
where

import qualified Numeric.Natural as Natural

newtype Seconds
  = Seconds Natural.Natural
  deriving (Eq, Ord, Show)

fromNatural :: Natural.Natural -> Seconds
fromNatural = Seconds

toNatural :: Seconds -> Natural.Natural
toNatural (Seconds natural) = natural
