-- | Payee parsers. There are two types of payee parsers:
--
-- Quoted payees. These allow the most latitude in the characters
-- allowed. They are surrounded by @<@ and @>@.
--
-- Unquoted payees. These are not surrounded by @<@ and @>@. Their
-- first character must be a letter or number.

module Penny.Copper.Payees (
  -- * Parse any payee
  payee

  -- * Quoted payees
  , quotedChar
  , quotedPayee

    -- * Unquoted payees
  , unquotedFirstChar
  , unquotedRestChars
  , unquotedPayee

    -- * Rendering
  , smartRender
  , quoteRender
  ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (guard)
import Data.Text (pack, Text)
import qualified Data.Text as X
import Text.Parsec (char, satisfy, many, between, (<?>), many1)
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Util as U
import qualified Penny.Lincoln.Bits as B

quotedChar :: Char -> Bool
quotedChar c = allowed && not banned where
  allowed = U.unicodeAll c && U.asciiAll c
  banned = c == '>'

payee :: Parser B.Payee
payee = quotedPayee <|> unquotedPayee

quotedPayee :: Parser B.Payee
quotedPayee = between (char '<') (char '>') p <?> "quoted payee"
  where
    p = (B.Payee . X.pack) <$> (many1 (satisfy quotedChar))

unquotedFirstChar :: Char -> Bool
unquotedFirstChar c = U.letter c || U.unicodeAll c

unquotedRestChars :: Char -> Bool
unquotedRestChars c = U.unicodeAll c || U.asciiAll c

unquotedPayee :: Parser B.Payee
unquotedPayee =
  let p c cs = B.Payee (pack (c:cs))
  in p
     <$> satisfy unquotedFirstChar
     <*> many (satisfy unquotedRestChars)
     <?> "unquoted payee"

renderUnquoted :: B.Payee -> Maybe Text
renderUnquoted (B.Payee p) = do
  (c1, cs) <- X.uncons p
  guard (unquotedFirstChar c1)
  guard (X.all unquotedRestChars cs)
  return p

quoteRender :: B.Payee -> Maybe Text
quoteRender (B.Payee p) = do
  guard (not . X.null $ p)
  guard (X.all quotedChar p)
  return $ '<' `X.cons` p `X.snoc` '>'

-- | Render a payee with a minimum of quoting. Fails if cannot be
-- rendered at all.
smartRender :: B.Payee -> Maybe Text
smartRender p = renderUnquoted p <|> quoteRender p
