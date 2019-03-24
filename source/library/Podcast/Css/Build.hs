module Podcast.Css.Build
  ( rule
  , declaration
  )
where

import qualified Podcast.Type.Css.Declaration as Declaration
import qualified Podcast.Type.Css.Rule as Rule

rule :: String -> [(String, String)] -> Rule.Rule
rule selector declarations = Rule.Rule
  { Rule.declarations = map declaration declarations
  , Rule.selector = selector
  }

declaration :: (String, String) -> Declaration.Declaration
declaration (property, value) = Declaration.Declaration
  { Declaration.property = property
  , Declaration.value = value
  }
