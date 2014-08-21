module Penny.Copper.Render where

import Data.Text
import Text.Parsec.Text (Parser)

-- | Things that can be rendered.  Properties of renderable items:
--
-- * 'render' is an injective function:
-- <http://en.wikipedia.org/wiki/Injective_function>
--
-- * 'render' never produces a null 'Text'
--
-- * The function
--
-- @
--   let parser x = 'parse' x 'Control.Applicative.<*' 'Text.Parsec.Combinator.eof' in
--   'Data.Either.Combinators.fromRight' '.' 'Text.Parsec.Prim.parse' 'parser' \"\"
-- @
--
-- is the left inverse of 'render'.

class Renderable a where
  render :: a -> Text
  parse :: Parser a

