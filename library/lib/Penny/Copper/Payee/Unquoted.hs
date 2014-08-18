module Penny.Copper.Payee.Unquoted
  ( UnquotedPayee
  , unUnquotedPayee
  , payeeToUnquotedPayee
  ) where

-- Imports

import Text.Parsec
import Penny.Common
import qualified Data.Text as X
import Penny.Copper.Render

-- | An unquoted payee may appear only in a TopLine.  The first
-- character may not be an opening curly brace, opening parenthesis,
-- open square brace, space, or newline.  Subsequent characters may
-- not be a newline.  Can be completely empty.

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
  Nothing -> Just (UnquotedPayee (Payee X.empty))
  Just (a, as)
    | bannedFirstChar a -> Nothing
    | X.all (not . bannedOtherChar) as -> Just (UnquotedPayee (Payee x))
    | otherwise -> Nothing

instance Renderable UnquotedPayee where
  render (UnquotedPayee (Payee x)) = x
  parse = do
    l1 <- optionMaybe (satisfy (not . bannedFirstChar))
    case l1 of
      Nothing -> return (UnquotedPayee (Payee X.empty))
      Just l -> do
        ls <- many (satisfy (not . bannedOtherChar))
        return . UnquotedPayee . Payee $
          X.cons l (X.pack ls)
