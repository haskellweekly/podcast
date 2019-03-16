module Podcast.Xml
  ( Root
  , Node
  , root
  , node
  , text
  , render
  )
where

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Podcast.Type.Xml.Attribute as Attribute
import qualified Podcast.Type.Xml.Element as Element
import qualified Podcast.Type.Xml.Node as Node

newtype Root
  = Root (Element.Element Node)
  deriving (Eq, Ord, Show)

newtype Node
  = Node (Node.Node Root)
  deriving (Eq, Ord, Show)

root :: String -> [(String, String)] -> [Node] -> Root
root name attributes nodes = Root Element.Element
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
node name attributes nodes = Node (Node.Element (root name attributes nodes))

text :: String -> Node
text string = Node (Node.Content (Text.pack string))

render :: Root -> String
render (Root element) =
  "<?xml version='1.0'?>" <> Text.unpack (renderElement element)

renderElement :: Element.Element Node -> Text.Text
renderElement element = let name = Element.name element in Text.concat
  [ Text.singleton '<'
  , name
  , renderAttributes (Element.attributes element)
  , let nodes = Element.nodes element in if Vector.null nodes
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
renderNode (Node node_) = case node_ of
  Node.Content text_ -> escape text_
  Node.Element (Root element) -> renderElement element

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
