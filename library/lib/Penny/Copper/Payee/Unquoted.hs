module Penny.Copper.Payee.Unquoted
  ( UnquotedPayee
  , unUnquotedPayee
  , payeeToUnquotedPayee
  ) where

-- Imports

import Text.Parsec
import Control.Monad
import Penny.Common
import qualified Data.Text as X
import Penny.Copper.Render

-- | An unquoted payee may appear only in a TopLine.  The first
-- character may not be an opening curly brace, opening parenthesis,
-- open square brace, space, or newline.  Subsequent characters may
-- not be a newline.  Must have at least one character.

newtype UnquotedPayee = UnquotedPayee { unUnquotedPayee :: Payee }
  deriving (Eq, Ord, Show)

bannedFirstChar :: Char -> Bool
bannedFirstChar c
  = c == '{'
  || c == '('
  || c == '{'
  || c == '['
  || c == ' '
  || c == '\n'

bannedOtherChar :: Char -> Bool
bannedOtherChar c
  = c == '\n'

payeeToUnquotedPayee :: Payee -> Maybe UnquotedPayee
payeeToUnquotedPayee (Payee x) = case X.uncons x of
  Nothing -> Nothing
  Just (a, as)
    | bannedFirstChar a -> Nothing
    | X.all (not . bannedOtherChar) as -> Just (UnquotedPayee (Payee x))
    | otherwise -> Nothing

instance Renderable UnquotedPayee where
  render (UnquotedPayee (Payee x)) = x
  parse = liftM2 f (satisfy (not . bannedFirstChar))
                   (many (satisfy (not . bannedOtherChar)))
    where
      f l1 ls = UnquotedPayee (Payee (X.cons l1 (X.pack ls)))
