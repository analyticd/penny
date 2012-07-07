-- | Copper - the Penny parser
module Penny.Copper (
  -- * Comments
  C.Comment(Comment),

  -- * Radix and grouping
  Q.RadGroup,
  Q.periodComma, Q.periodSpace, Q.commaPeriod, Q.commaSpace,
  Q.GroupingSpec(..),
  
  -- * Default time zone
  DT.DefaultTimeZone(DefaultTimeZone),
  DT.utcDefault,
  
  -- * FileContents
  FileContents(FileContents, unFileContents),
  
  -- * Metadata
  M.TopLineLine(unTopLineLine),
  M.TopMemoLine(unTopMemoLine),
  M.Side(CommodityOnLeft, CommodityOnRight),
  M.SpaceBetween(SpaceBetween, NoSpaceBetween),
  M.Format(Format, side, between),
  M.Filename(Filename, unFilename),
  M.PriceLine(unPriceLine),
  M.PostingLine(unPostingLine),
  M.PriceMeta(priceLine, priceFormat),
  M.GlobalPosting(unGlobalPosting),
  M.FilePosting(unFilePosting),
  M.PostingMeta(postingLine, postingFormat, globalPosting,
                filePosting),
  M.GlobalTransaction(unGlobalTransaction),
  M.TopLineMeta(topMemoLine, topLineLine, filename,
                globalTransaction, fileTransaction),
  
  -- * Errors
  ErrorMsg (ErrorMsg, unErrorMsg),

  -- * Items
  I.Item(Transaction, Price, CommentItem, BlankLine),
  I.Line(unLine),

  -- * Parsing
  Ledger(Ledger, unLedger),
  parse,
  
  -- * Rendering
  I.render
  ) where


import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import Text.Parsec ( manyTill, eof )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Meta as M
import qualified Penny.Copper.Comments as C
import qualified Penny.Copper.Qty as Q
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Item as I
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Serial as S
import qualified Penny.Lincoln.Family as F

data Ledger =
  Ledger { unLedger :: [(I.Line, I.Item M.TopLineMeta M.PostingMeta)] }
  deriving Show

newtype FileContents = FileContents { unFileContents :: X.Text }
                       deriving (Eq, Show)

newtype ErrorMsg = ErrorMsg { unErrorMsg :: X.Text }
                   deriving (Eq, Show)

type TopLineFileMeta =
  (M.TopMemoLine, M.TopLineLine, M.Filename, M.FileTransaction)

type PostingFileMeta =
  (M.PostingLine, Maybe M.Format, M.FilePosting)

parseFile ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> M.Filename
  -> Parser [(I.Line, I.Item TopLineFileMeta PostingFileMeta)]
parseFile dtz rg fn =
  addFileMetadata fn
  <$> manyTill (I.itemWithLineNumber dtz rg) eof

addFileMetadata ::
  M.Filename
  -> [(I.Line, (I.Item (M.TopMemoLine, M.TopLineLine)
                (M.PostingLine, Maybe M.Format)))]
  -> [(I.Line, I.Item TopLineFileMeta PostingFileMeta)]
addFileMetadata fn ls =
  let (lns, is) = (map fst ls, map snd ls)
      eis = map toEiItem is
      procTop s = append3 (M.FileTransaction s) . append2 fn
      procPstg s = append2 (M.FilePosting s)
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in zip lns is'


parseFiles ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> [(M.Filename, FileContents)]
  -> Parser [(I.Line, I.Item M.TopLineMeta M.PostingMeta)]
parseFiles = undefined

parse ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> [(M.Filename, FileContents)]
  -> Ex.Exceptional ErrorMsg Ledger
parse = undefined

append1 :: b -> a -> (a, b)
append1 b a = (a, b)

append2 :: c -> (a, b) -> (a, b, c)
append2 c (a, b) = (a, b, c)

append3 :: d -> (a, b, c) -> (a, b, c, d)
append3 d (a, b, c) = (a, b, c, d)

append4 :: e -> (a, b, c, d) -> (a, b, c, d, e)
append4 e (a, b, c, d) = (a, b, c, d, e)

data Other = OPrice (L.PricePoint M.PriceMeta)
             | OCommentItem C.Comment
             | OBlankLine
             deriving Show

type EiItem tm pm = Either Other (L.Transaction tm pm)

toEiItem :: I.Item tm pm -> EiItem tm pm
toEiItem i = case i of
  I.Transaction t -> Right t
  I.Price p -> Left (OPrice p)
  I.CommentItem c -> Left (OCommentItem c)
  I.BlankLine -> Left OBlankLine

fromEiItem :: EiItem tm pm -> I.Item tm pm
fromEiItem i = case i of
  Left l -> case l of
    OPrice p -> I.Price p
    OCommentItem c -> I.CommentItem c
    OBlankLine -> I.BlankLine
  Right t -> I.Transaction t

