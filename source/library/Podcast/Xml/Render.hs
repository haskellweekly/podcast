module Podcast.Xml.Render
  ( element
  , attributes
  , attribute
  , nodes
  , node
  , text
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node

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
  , text (Attribute.value attribute_)
  , "'"
  ]

nodes :: [Element.Node] -> String
nodes = concatMap node

node :: Element.Node -> String
node node_ = case node_ of
  Node.Content content -> text content
  Node.Element element_ -> element element_

text :: String -> String
text = concatMap escape

escape :: Char -> String
escape char = case char of
  '\x22' -> "&quot;"
  '\x26' -> "&amp;"
  '\x27' -> "&apos;"
  '\x3c' -> "&lt;"
  '\x3e' -> "&gt;"
  _ -> [char]
