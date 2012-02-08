module Penny.Zinc.Parser.Sorter where

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid.Extra (Orderer(Orderer))
import Data.Text (Text, pack)

import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Lincoln.Queries as Q

ordering ::
  (Ord r)
  => (PostingBox t p -> r)
  -> Orderer (PostingBox t p)
ordering q = Orderer f where
  f p1 p2 = compare (q p1) (q p2)

flipOrder :: Orderer (PostingBox t p) -> Orderer (PostingBox t p)
flipOrder (Orderer f) = Orderer f' where
  f' p1 p2 = case f p1 p2 of
    LT -> GT
    GT -> LT
    EQ -> EQ

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter s = case s of
  [] -> []
  (x:xs) -> toUpper x : xs

ords :: Map Text (Orderer (PostingBox t p))
ords = M.fromList (lowers ++ uppers) where
  uppers = map toReversed ordPairs
  toReversed (s, f) =
    (pack . capitalizeFirstLetter $ s, flipOrder f)
  lowers = map toPair ordPairs
  toPair (s, f) = (pack s, f)

ordPairs :: [(String, Orderer (PostingBox t p))]
ordPairs = 
  [ ("payee", ordering Q.payee)
  , ("date", ordering Q.dateTime)
  , ("flag", ordering Q.flag)
  , ("number", ordering Q.number)
  , ("account", ordering Q.account)
  , ("drCr", ordering Q.drCr)
  , ("qty", ordering Q.qty)
  , ("commodity", ordering Q.commodity)
  , ("postingMemo", ordering Q.postingMemo)
  , ("transactionMemo", ordering Q.transactionMemo) ]
