module Penny.Zinc.Parser.Sorter where

import Data.Char (toUpper)
import Data.Monoid (mappend)
import Data.Monoid.Extra (Orderer(Orderer))
import Data.Text (Text, pack, isPrefixOf)
import System.Console.MultiArg.Prim (ParserE, throw)
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option (makeShortOpt, makeLongOpt)

import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Lincoln.Queries as Q

import Penny.Zinc.Parser.Error (Error(BadSortKeyError))

ordering ::
  (Ord r)
  => (PostingBox -> r)
  -> Orderer PostingBox
ordering q = Orderer f where
  f p1 p2 = compare (q p1) (q p2)

flipOrder :: Orderer PostingBox -> Orderer PostingBox
flipOrder (Orderer f) = Orderer f' where
  f' p1 p2 = case f p1 p2 of
    LT -> GT
    GT -> LT
    EQ -> EQ

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter s = case s of
  [] -> []
  (x:xs) -> toUpper x : xs

ords :: [(Text, Orderer PostingBox)]
ords = lowers ++ uppers where
  uppers = map toReversed ordPairs
  toReversed (s, f) =
    (pack . capitalizeFirstLetter $ s, flipOrder f)
  lowers = map toPair ordPairs
  toPair (s, f) = (pack s, f)

ordPairs :: [(String, Orderer PostingBox)]
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

sort :: Orderer PostingBox
        -> ParserE Error (Orderer PostingBox)
sort ordIn = do
  let lo = makeLongOpt . pack $ "sort"
      so = makeShortOpt 's'
  (_, arg) <- mixedOneArg lo [] [so]
  let matches = filter (\p -> arg `isPrefixOf` (fst p)) ords
  sorter <- case matches of
    [] -> throw $ BadSortKeyError arg
    x:[] -> return $ snd x
    _ -> throw $ BadSortKeyError arg
  return $ mappend ordIn sorter
