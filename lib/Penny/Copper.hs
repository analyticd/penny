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
  
  -- * Filename and FileContents
  Filename(Filename, unFilename),
  FileContents(FileContents, unFileContents),
  
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


import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import Text.Parsec ( manyTill, eof )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Meta as M
import qualified Penny.Copper.Comments as C
import qualified Penny.Copper.Qty as Q
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Item as I

data Ledger =
  Ledger { unLedger :: [(I.Line, I.Item M.TopLineMeta M.PostingMeta)] }
  deriving Show

newtype Filename = Filename { unFilename :: X.Text }
                   deriving (Eq, Show)

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
  -> (Filename, FileContents)
  -> Parser [(I.Line, I.Item TopLineFileMeta PostingFileMeta)]
parseFile = undefined

parseFiles ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> [(Filename, FileContents)]
  -> Parser [(I.Line, I.Item M.TopLineMeta M.PostingMeta)]
parseFiles = undefined

parse ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> [(Filename, FileContents)]
  -> Ex.Exceptional ErrorMsg Ledger
parse = undefined
