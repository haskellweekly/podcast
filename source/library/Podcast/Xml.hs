module Podcast.Xml
  ( Element.Element
  , Node
  , element
  , node
  , text
  , render
  , renderElement
  )
where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node

type Node = Node.Node Element.Element

element :: String -> [(String, String)] -> [Node] -> Element.Element
element name attributes nodes = Element.Element
  { Element.name = Text.pack name
  , Element.attributes = Vector.fromList (map attribute attributes)
  , Element.nodes = Vector.fromList nodes
  }

attribute :: (String, String) -> Attribute.Attribute
attribute (name, value) = Attribute.Attribute
  { Attribute.name = Text.pack name
  , Attribute.value = Text.pack value
  }

node :: String -> [(String, String)] -> [Node] -> Node
node name attributes nodes = Node.Element (element name attributes nodes)

text :: String -> Node
text string = Node.Content (Text.pack string)

render :: Element.Element -> String
render element_ = "<?xml version='1.0'?>" <> Text.unpack (renderElement element_)

renderElement :: Element.Element -> Text.Text
renderElement element_ = let name = Element.name element_ in Text.concat
  [ Text.singleton '<'
  , name
  , renderAttributes (Element.attributes element_)
  , let nodes = Element.nodes element_ in if Vector.null nodes
    then Text.pack " />"
    else Text.concat
      [ Text.singleton '>'
      , renderNodes nodes
      , Text.pack "</"
      , name
      , Text.singleton '>'
      ]
  ]

renderAttributes :: Vector.Vector Attribute.Attribute -> Text.Text
renderAttributes attributes = if Vector.null attributes
  then Text.empty
  else Text.cons ' ' (Text.unwords (map renderAttribute (Vector.toList attributes)))

renderAttribute :: Attribute.Attribute -> Text.Text
renderAttribute attribute_ = Text.concat
  [ Attribute.name attribute_
  , Text.pack "='"
  , escape (Attribute.value attribute_)
  , Text.singleton '\''
  ]

renderNodes :: Vector.Vector Node -> Text.Text
renderNodes nodes = Text.concat (map renderNode (Vector.toList nodes))

renderNode :: Node -> Text.Text
renderNode node_ = case node_ of
  Node.Content text_ -> escape text_
  Node.Element element_ -> renderElement element_

escape :: Text.Text -> Text.Text
escape = Text.concatMap escapeChar

escapeChar :: Char -> Text.Text
escapeChar char = case char of
  '\x22' -> Text.pack "&quot;"
  '\x26' -> Text.pack "&amp;"
  '\x27' -> Text.pack "&apos;"
  '\x3c' -> Text.pack "&lt;"
  '\x3e' -> Text.pack "&gt;"
  _ -> Text.singleton char
