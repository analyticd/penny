module Penny.Liberty.Sorter where


import Data.Char (toUpper)
import Data.Text (Text, pack, isPrefixOf)
import System.Console.MultiArg.Prim (ParserE, throw)
import System.Console.MultiArg.Combinator
import System.Console.MultiArg.Option (makeShortOpt, makeLongOpt)

import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Lincoln.Queries as Q

import Penny.Liberty.Error (Error(BadSortKeyError))

-- The order of the arguments to mappend is correct - the functions
-- will run right to left, like a compose
sort :: ParserE Error Orderer
sort = do
  let lo = makeLongOpt . pack $ "sort"
      so = makeShortOpt 's'
  (_, arg) <- mixedOneArg lo [] [so]
  let matches = filter (\p -> arg `isPrefixOf` (fst p)) ords
  case matches of
    [] -> throw $ BadSortKeyError arg
    x:[] -> return $ snd x
    _ -> throw $ BadSortKeyError arg


