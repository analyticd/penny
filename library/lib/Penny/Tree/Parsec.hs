module Penny.Tree.Parsec
  ( module Text.Parsec
  , module Control.Monad
  , module Control.Applicative
  , module Text.Parsec.Text
  , seq
  , accept
  , yarn
  , either
  , novem
  , decem
  , radixPeriod
  , radixComma
  ) where

import Text.Parsec hiding (optional)
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Text
import Data.Sequence (Seq, fromList)
import Prelude hiding (seq, either)
import Text.Parsec.Pos (updatePosChar)
import Deka.Native.Abstract
import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Core.Anna.RadPer as RadPer

seq :: Parser a -> Parser (Seq a)
seq p = fmap fromList $ Text.Parsec.many p

accept
  :: String
  -- ^ Description, for error messages
  -> (Char -> Maybe a)
  -> Parser a
accept d f = tokenPrim (\c -> [c]) (\ps c _ -> updatePosChar ps c)
  f <?> d

yarn :: String -> (Char -> Maybe a) -> Parser (Seq a)
yarn s f = seq (accept s f)

either :: Parser a -> Parser b -> Parser (Either a b)
either a b = fmap Left a <|> fmap Right b

novem :: Parser Novem
novem = choice
  [ D1 <$ char '1'
  , D2 <$ char '2'
  , D3 <$ char '3'
  , D4 <$ char '4'
  , D5 <$ char '5'
  , D6 <$ char '6'
  , D7 <$ char '7'
  , D8 <$ char '8'
  , D9 <$ char '9'
  ]

decem :: Parser Decem
decem = (D0 <$ char '0') <|> fmap Nonem novem

radixPeriod :: Parser (Radix.T RadPer.T)
radixPeriod = fmap (const Radix.T) $ char '.'

radixComma :: Parser (Radix.T RadCom.T)
radixComma = fmap (const Radix.T) $ char ','
