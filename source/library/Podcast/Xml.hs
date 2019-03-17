module Podcast.Xml
  ( Element.Element
  , Element.Node
  , Build.element
  , Build.node
  , Build.text
  , render
  )
where

import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Xml.Build as Build
import qualified Podcast.Xml.Render as Render

render :: Element.Element -> String
render element = "<?xml version='1.0'?>" ++ Render.element element
