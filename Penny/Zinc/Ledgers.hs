module Penny.Zinc.Ledgers where

import Control.Applicative ((<$>), many, pure, (<*>), liftA)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)
import qualified Penny.Liberty.Error as E
import System.Console.MultiArg.Combinator (option)
import System.Console.MultiArg.Prim (ParserE, nextArg, try)

data Filename =
  Filename Text
  | Stdin

filename :: ParserE E.Error Filename
filename = do
  a <- nextArg
  return $ if a == pack "-"
           then Stdin
           else Filename a

filenames :: ParserE E.Error (NE.NonEmpty Filename)
filenames = do
  fn1 <- option Stdin (Filename <$> nextArg)
  fns <- option [] $ liftA (fmap Filename) (many nextArg)
  return $ NE.nonEmpty fn1 fns
