module Podcast.Type.Bytes
  ( Bytes
  , fromNatural
  , toNatural
  , toString
  )
where

import qualified Numeric.Natural as Natural

newtype Bytes
  = Bytes Natural.Natural
  deriving (Eq, Ord, Show)

fromNatural :: Natural.Natural -> Bytes
fromNatural = Bytes

toNatural :: Bytes -> Natural.Natural
toNatural (Bytes natural) = natural

toString :: Bytes -> String
toString bytes = show (toNatural bytes)
