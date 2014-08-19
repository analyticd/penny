{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Account.Unquoted
  ( UnquotedAccount
  , accountToUnquotedAccount
  , unUnquotedAccount
  ) where

import Control.Applicative hiding (many)
import Data.Sequence
import Data.List (intersperse)
import qualified Data.Text as X
import Data.Char
import qualified Data.Foldable as F
import Penny.Posting
import Penny.Copper.Render
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | An unquoted account appears with no surrounding quotes.  The
-- first character of the first sub-account must be a letter.  The
-- account name must not be empty.

newtype UnquotedAccount
  = UnquotedAccount { unUnquotedAccount :: Account }
  deriving (Eq, Ord, Show)

goodAccount :: Account -> Bool
goodAccount (Account as) = case viewl as of
  EmptyL -> False
  SubAccount x :< _ -> goodFirstLetter && goodRest
    where
      goodFirstLetter = case X.uncons x of
        Nothing -> False
        Just (l, _) -> isLetter l
      goodRest = F.all (X.all (not . banned)) . fmap unSubAccount $ as

accountToUnquotedAccount :: Account -> Maybe UnquotedAccount
accountToUnquotedAccount a
  | goodAccount a = Just (UnquotedAccount a)
  | otherwise = Nothing

banned :: Char -> Bool
banned c
  = c == ':'
  || c == ' '
  || c == '\n'

renderAccount :: Account -> X.Text
renderAccount = X.concat . intersperse ":" . F.toList
    . fmap unSubAccount . unAccount

parseAccount :: Parser Account
parseAccount =
  (Account . fromList)
  <$> ((:) <$> firstSubAccount <*> many nextSubAccount)
  where
    nextSubAccount = char ':' *> nonFirstSubAccount

firstSubAccount :: Parser SubAccount
firstSubAccount =
  (SubAccount . X.pack)
  <$> ((:) <$> letter <*> many (satisfy (not . banned)))

nonFirstSubAccount :: Parser SubAccount
nonFirstSubAccount =
  (SubAccount . X.pack)
  <$> (many (satisfy (not . banned)))

instance Renderable UnquotedAccount where
  render = flip X.snoc ' ' . renderAccount . unUnquotedAccount
  parse = fmap UnquotedAccount $ parseAccount <* char ' '

