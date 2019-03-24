module Podcast.Type.Css.Rule
  ( Rule(..)
  )
where

import qualified Podcast.Type.Css.Declaration as Declaration

data Rule = Rule
  { declarations :: [Declaration.Declaration]
  , selector :: String
  }
