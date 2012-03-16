-- | Payee parsers. There are two types of payee parsers:
--
-- Quoted payees. These allow the most latitude in the characters
-- allowed. They are surrounded by @<@ and @>@.
--
-- Unquoted payees. These are not surrounded by @<@ and @>@. Their
-- first character must be a letter or number.

module Penny.Copper.Payees (
  -- * Quoted payees
  quotedChar
  , quotedPayee
    
    -- * Unquoted payees
  , unquotedFirstChar
  , unquotedRestChars
  , unquotedPayee
    
    -- * Rendering
  , render
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Char as C
import Data.Text (pack, Text)
import Text.Parsec (char, satisfy, many, between, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

quotedChar :: Char -> Bool
quotedChar c = allowed && not banned where
  allowed = inCat C.UppercaseLetter C.OtherSymbol c ||
            c == ' '
  banned = c == '>'

quotedPayee :: Parser B.Payee
quotedPayee = between (char '<') (char '>') p <?> "quoted payee" where
  p = (\c cs -> B.Payee (TextNonEmpty c (pack cs)))
      <$> satisfy quotedChar
      <*> many (satisfy quotedChar)

unquotedFirstChar :: Char -> Bool
unquotedFirstChar = inCat C.UppercaseLetter C.OtherLetter

unquotedRestChars :: Char -> Bool
unquotedRestChars c = inCat C.UppercaseLetter C.OtherSymbol c
                      || c == ' '

unquotedPayee :: Parser B.Payee
unquotedPayee = let
  p c cs = B.Payee (TextNonEmpty c (pack cs))
  in p
     <$> satisfy unquotedFirstChar
     <*> many (satisfy unquotedRestChars)
     <?> "unquoted payee"

-- | Render a payee with a minimum of quoting. Fails if cannot be
-- rendered at all.
render :: B.Payee -> Maybe Text
render = undefined
