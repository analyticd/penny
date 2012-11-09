-- | Copper - the Penny parser
module Penny.Copper
  ( -- * Comments
    C.Comment(Comment)

  -- * Radix and grouping
  , Q.GroupingSpec(..)

  -- * FileContents
  , FileContents(FileContents, unFileContents)

  -- * Errors
  , ErrorMsg (ErrorMsg, unErrorMsg)

  -- * Items
  , I.Item(Transaction, Price, CommentItem, BlankLine)
  , I.Line(unLine)

  -- * Parsing
  , Ledger(Ledger, unLedger)
  , parse

  -- * Rendering
  , I.render
  ) where


import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import Text.Parsec ( manyTill, eof )
import qualified Text.Parsec as P

import qualified Penny.Copper.Comments as C
import qualified Penny.Copper.Qty as Q
import qualified Penny.Copper.Item as I
import qualified Penny.Lincoln as L

data Ledger =
  Ledger { unLedger :: [(I.Line, I.Item)] }
  deriving Show

newtype FileContents = FileContents { unFileContents :: X.Text }
                       deriving (Eq, Show)

newtype ErrorMsg = ErrorMsg { unErrorMsg :: X.Text }
                   deriving (Eq, Show)

parseFile
  :: (L.Filename, FileContents)
  -> Ex.Exceptional ErrorMsg [(I.Line, I.Item)]
parseFile (fn, (FileContents c)) =
  let p = addFileMetadata fn
          <$> manyTill I.itemWithLineNumber eof
      fnStr = X.unpack . L.unFilename $ fn
  in case P.parse p fnStr c of
    Left err -> Ex.throw (ErrorMsg . X.pack . show $ err)
    Right g -> return g

addFileMetadata ::
  L.Filename
  -> [(I.Line, I.Item)]
  -> [(I.Line, I.Item)]
addFileMetadata fn ls =
  let (lns, is) = (map fst ls, map snd ls)
      eis = map toEiItem is
      procTop s m =
        m { L.fileTransaction = Just (L.FileTransaction s)
          , L.filename = Just fn }
      procPstg s m =
        m { L.filePosting = Just (L.FilePosting s) }
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in zip lns is'


addGlobalMetadata ::
  [[(I.Line, I.Item)]]
  -> [(I.Line, I.Item)]
addGlobalMetadata lss =
  let ls = concat lss
      procTop s m =
        m { L.globalTransaction = Just (L.GlobalTransaction s) }
      procPstg s m =
        m { L.globalPosting = Just (L.GlobalPosting s) }
      (lns, is) = (map fst ls, map snd ls)
      eis = map toEiItem is
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in zip lns is'

parse
  :: [(L.Filename, FileContents)]
  -> Ex.Exceptional ErrorMsg Ledger
parse ps =
  mapM parseFile ps
  >>= (return . Ledger . addGlobalMetadata)

data Other = OPrice L.PricePoint
             | OCommentItem C.Comment
             | OBlankLine
             deriving Show

type EiItem = Either Other L.Transaction

toEiItem :: I.Item -> EiItem
toEiItem i = case i of
  I.Transaction t -> Right t
  I.Price p -> Left (OPrice p)
  I.CommentItem c -> Left (OCommentItem c)
  I.BlankLine -> Left OBlankLine

fromEiItem :: EiItem -> I.Item
fromEiItem i = case i of
  Left l -> case l of
    OPrice p -> I.Price p
    OCommentItem c -> I.CommentItem c
    OBlankLine -> I.BlankLine
  Right t -> I.Transaction t

