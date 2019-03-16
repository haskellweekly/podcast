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

fromNatural :: Natural.Natural -> Number
fromNatural = Number

toNatural :: Number -> Natural.Natural
toNatural (Number natural) = natural
