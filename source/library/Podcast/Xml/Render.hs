module Podcast.Xml.Render
  ( renderElement
  , renderAttributes
  , renderAttribute
  , renderNodes
  , renderNode
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node
import qualified Podcast.Xml.Escape as Escape

renderElement :: Element.Element -> String
renderElement element = let
  name = Element.name element
  attributes = Element.attributes element
  nodes = Element.nodes element
  in concat
    [ "<"
    , name
    , renderAttributes attributes
    , if null nodes
      then " />"
      else concat [">", renderNodes nodes, "</", name, ">"]
    ]

renderAttributes :: [Attribute.Attribute] -> String
renderAttributes attributes = if null attributes
  then ""
  else ' ' : unwords (map renderAttribute attributes)

renderAttribute :: Attribute.Attribute -> String
renderAttribute attribute = concat
  [ Attribute.name attribute
  , "='"
  , Escape.escape (Attribute.value attribute)
  , "'"
  ]

renderNodes :: [Element.Node] -> String
renderNodes = concatMap renderNode

renderNode :: Element.Node -> String
renderNode node = case node of
  Node.Content content -> Escape.escape content
  Node.Element element -> renderElement element
