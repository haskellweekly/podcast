module Podcast.Xml
  ( Element.Element
  , Element.Node
  , element
  , node
  , text
  , render
  , renderElement
  )
where

import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node

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
render element_ = "<?xml version='1.0'?>" <> renderElement element_

renderElement :: Element.Element -> String
renderElement element_ = let name = Element.name element_ in concat
  [ "<"
  , name
  , renderAttributes (Element.attributes element_)
  , let nodes = Element.nodes element_ in if null nodes
    then " />"
    else concat
      [ ">"
      , renderNodes nodes
      , "</"
      , name
      , ">"
      ]
  ]

renderAttributes :: [Attribute.Attribute] -> String
renderAttributes attributes = if null attributes
  then ""
  else ' ' : unwords (map renderAttribute attributes)

renderAttribute :: Attribute.Attribute -> String
renderAttribute attribute_ = concat
  [ Attribute.name attribute_
  , "='"
  , escape (Attribute.value attribute_)
  , "'"
  ]

renderNodes :: [Element.Node] -> String
renderNodes = concatMap renderNode

renderNode :: Element.Node -> String
renderNode node_ = case node_ of
  Node.Content text_ -> escape text_
  Node.Element element_ -> renderElement element_

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar char = case char of
  '\x22' -> "&quot;"
  '\x26' -> "&amp;"
  '\x27' -> "&apos;"
  '\x3c' -> "&lt;"
  '\x3e' -> "&gt;"
  _ -> [char]
