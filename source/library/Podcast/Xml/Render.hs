module Podcast.Xml.Render
  ( element
  , attributes
  , attribute
  , nodes
  , node
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node
import qualified Podcast.Xml.Escape as Escape

element :: Element.Element -> String
element element_ =
  let
    name = Element.name element_
    nodes_ = Element.nodes element_
  in concat
    [ "<"
    , name
    , attributes (Element.attributes element_)
    , if null nodes_
      then " />"
      else concat [">", nodes nodes_, "</", name, ">"]
    ]

attributes :: [Attribute.Attribute] -> String
attributes attributes_ =
  if null attributes_ then "" else ' ' : unwords (map attribute attributes_)

attribute :: Attribute.Attribute -> String
attribute attribute_ = concat
  [ Attribute.name attribute_
  , "='"
  , Escape.escape (Attribute.value attribute_)
  , "'"
  ]

nodes :: [Element.Node] -> String
nodes = concatMap node

node :: Element.Node -> String
node node_ = case node_ of
  Node.Content content -> Escape.escape content
  Node.Element element_ -> element element_
