module Podcast.Html
  ( Xml.Element
  , Xml.Node
  , Xml.element
  , Xml.node
  , Xml.text
  , render
  )
where

import qualified Podcast.Xml as Xml
import qualified Podcast.Xml.Render as Render

render :: Xml.Element -> String
render element = "<!doctype html>" ++ Render.element element
