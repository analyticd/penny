module Penny.Copper.Render where

import Data.Text
import Text.Parsec.Text (Parser)

class Renderable a where
  render :: a -> Text
  parse :: Parser a

