{-# LANGUAGE FlexibleInstances #-}
module Penny.Copper.Render where

import Data.Text (Text)
import qualified Data.Text as X
import Text.Parsec.Text (Parser)
import Deka.Native.Abstract
import Text.Parsec.Pos
import Text.Parsec

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
--   let run x = 'parser' x 'Control.Applicative.<*' 'Text.Parsec.Combinator.eof' in
--   'Data.Either.Combinators.fromRight' '.' 'Text.Parsec.Prim.parse' 'run' \"\"
-- @
--
-- is the left inverse of 'render'.
--
-- * 'parser' does not accept an empty string; that is, if 'parser'
-- succeeds, it consumes at least one character.

class Renderable a where
  render :: a -> Text
  parser :: Parser a

instance Renderable Novem where
  render = X.singleton . novemToChar
  parser = tokenPrim (:[]) (\ps t _ -> updatePosChar ps t)
    charToNovem

instance Renderable Decem where
  render = X.singleton . decemToChar
  parser = tokenPrim (:[]) (\ps t _ -> updatePosChar ps t)
    charToDecem

