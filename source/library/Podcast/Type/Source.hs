module Podcast.Type.Source
  ( Source(..)
  )
where

import qualified Podcast.Type.Xml.Element as Element

data Source
  = File FilePath
  | Html Element.Element
  | Xml Element.Element
  deriving (Eq, Ord, Show)
