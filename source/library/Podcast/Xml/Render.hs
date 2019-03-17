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
element e =
  let
    n = Element.name e
    as = Element.attributes e
    ns = Element.nodes e
  in concat
    [ "<"
    , n
    , attributes as
    , if null ns then " />" else concat [">", nodes ns, "</", n, ">"]
    ]

attributes :: [Attribute.Attribute] -> String
attributes as = if null as then "" else ' ' : unwords (map attribute as)

attribute :: Attribute.Attribute -> String
attribute a =
  concat [Attribute.name a, "='", Escape.escape (Attribute.value a), "'"]

nodes :: [Element.Node] -> String
nodes = concatMap node

node :: Element.Node -> String
node n = case n of
  Node.Content c -> Escape.escape c
  Node.Element e -> element e
