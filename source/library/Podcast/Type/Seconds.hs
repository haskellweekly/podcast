module Podcast.Type.Seconds
  ( Seconds
  , fromNatural
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

toNatural :: Seconds -> Natural.Natural
toNatural (Seconds natural) = natural

toString :: Seconds -> String
toString seconds =
  let (m, s) = quotRem (toNatural seconds) 60 in Printf.printf "%d:%02d" m s
