module Podcast.Type.Number
  ( Number
  , fromNatural
  , toNatural
  )
where

import qualified Numeric.Natural as Natural

newtype Number
  = Number Natural.Natural
  deriving (Eq, Ord, Show)

fromNatural :: Natural.Natural -> Either String Number
fromNatural natural = if natural == 0
  then Left ("invalid Number: " <> show natural)
  else Right (Number natural)

toNatural :: Number -> Natural.Natural
toNatural (Number natural) = natural
