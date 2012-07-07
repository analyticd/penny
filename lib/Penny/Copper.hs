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
addFileMetadata = undefined


addFileTransaction ::
  M.Filename
  -> S.Serial
  -> (M.TopMemoLine, M.TopLineLine)
  -> TopLineFileMeta
addFileTransaction fn s (tml, tll) =
  (tml, tll, fn, (M.FileTransaction s))

addFilePosting ::
  S.Serial
  -> (M.PostingLine, Maybe M.Format)
  -> PostingFileMeta
addFilePosting s (pl, fmt) = (pl, fmt, M.FilePosting s)

{-
fileItemSelect ::
  I.Item (M.TopMemoLine, M.TopLineLine)
  (M.PostingLine, Maybe M.Format)
  -> Either (F.Family (L.TopLine (M.TopMemoLine, M.TopLineLine))
             (L.Posting (M.PostingLine, Maybe M.Format)),
             F.Family (L.TopLine TopLineFileMeta)
             (L.Posting PostingFileMeta)
             -> I.Item TopLineFileMeta PostingFileMeta)
  (I.Item TopLineFileMeta PostingFileMeta)
fileItemSelect i = case i of
  I.Transaction t ->
    let fam = L.unTransaction t
        changer fam' = I.Transaction 
    in Left (fam, \f -> I.Transaction (L.changePostingMeta )
  I.Price p -> Right (I.Price p)
  I.CommentItem c -> Right (I.CommentItem c)
  I.BlankLine -> I.BlankLine
-}

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
