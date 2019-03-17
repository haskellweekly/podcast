module Podcast.Html
  ( Xml.Element
  , Xml.Node
  , Xml.element
  , Xml.node
  , Xml.text
  , render
  )
where

import qualified Data.Text as Text
import qualified Podcast.Xml as Xml

render :: Xml.Element -> String
render element = "<!doctype html>" <> Text.unpack (Xml.renderElement element)
