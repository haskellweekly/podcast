module Podcast.Xml
  ( Element.Element
  , Element.Node
  , element
  , node
  , text
  , render
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node
import qualified Podcast.Xml.Render as Render

element :: String -> [(String, String)] -> [Element.Node] -> Element.Element
element name attributes nodes = Element.Element
  { Element.name = name
  , Element.attributes = map attribute attributes
  , Element.nodes = nodes
  }

attribute :: (String, String) -> Attribute.Attribute
attribute (name, value) = Attribute.Attribute
  { Attribute.name = name
  , Attribute.value = value
  }

node :: String -> [(String, String)] -> [Element.Node] -> Element.Node
node name attributes nodes = Node.Element (element name attributes nodes)

text :: String -> Element.Node
text = Node.Content

render :: Element.Element -> String
render element_ = "<?xml version='1.0'?>" ++ Render.element element_
