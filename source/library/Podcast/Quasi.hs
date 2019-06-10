module Podcast.Quasi
  ( string
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Language.Haskell.TH.Quote as Quote
import qualified Language.Haskell.TH.Syntax as Syntax

string :: Quote.QuasiQuoter
string = Quote.QuasiQuoter
  { Quote.quoteDec = const
    (fail "cannot use [string|...|] quasi-quotation as a declaration")
  , Quote.quoteExp = pure . Syntax.LitE . Syntax.StringL . strip
  , Quote.quotePat = pure . Syntax.LitP . Syntax.StringL . strip
  , Quote.quoteType = pure . Syntax.LitT . Syntax.StrTyLit . strip
  }

strip :: String -> String
strip = List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace
