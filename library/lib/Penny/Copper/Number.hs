{-# LANGUAGE OverloadedStrings #-}

module Penny.Copper.Number
  ( Number
  , unNumber
  , toCopperNumber
  ) where

-- Imports

import qualified Penny.Common as C
import qualified Data.Text as X
import Data.Monoid
import Text.Parsec
import Penny.Copper.Render

-- | A Number is the same whether in a top line or posting.  Is always
-- surrounded by parentheses.  Can be completely empty.  Does not
-- contain a newline or a closing parenthesis.

newtype Number = Number { unNumber :: C.Number }
  deriving (Eq, Ord, Show)

toCopperNumber :: C.Number -> Maybe Number
toCopperNumber (C.Number n)
  | X.all (not . banned) n = Just (Number (C.Number n))
  | otherwise = Nothing

instance Renderable Number where
  render (Number (C.Number x)) = "(" <> x <> ")"
  parser = do
    _ <- char '('
    xs <- many (satisfy (not . banned))
    _ <- char ')'
    return . Number . C.Number . X.pack $ xs

banned :: Char -> Bool
banned c =
  c == '\n'
  || c == ')'
