module Penny.Copper.Util where

import Control.Applicative ((<*), pure)
import qualified Data.Char as C
import Data.Ix (range)
import qualified Data.Set as S
import Text.Parsec (char, many)
import Text.Parsec.Text (Parser)

inCat :: C.GeneralCategory -> C.GeneralCategory
         -> Char -> Bool
inCat g1 g2 c = C.generalCategory c `S.member` gs where
  gs = S.fromList (range (g1, g2))

-- | Creates a new parser that behaves like the old one, but also
-- parses any whitespace remaining afterward.
lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

-- | Parses any trailing whitespace followed by a newline followed by
-- additional whitespace.
eol :: Parser ()
eol = pure ()
      <* many (char ' ')
      <* char '\n'
      <* many (char ' ')
