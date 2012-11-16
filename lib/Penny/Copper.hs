-- | Copper - the Penny parser
module Penny.Copper
  ( parse
  , Y.Item(BlankLine, Comment, PricePoint, Transaction)
  , Y.Ledger(Ledger, unLedger)
  , FileContents(FileContents, unFileContents)
  , ErrorMsg (unErrorMsg)
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Text.Parsec as Parsec
import qualified Penny.Copper.Parsec as CP

import qualified Penny.Lincoln as L
import qualified Penny.Copper.Types as Y

newtype FileContents = FileContents { unFileContents :: X.Text }
                       deriving (Eq, Show)

newtype ErrorMsg = ErrorMsg { unErrorMsg :: X.Text }
                   deriving (Eq, Show)

parseFile ::
  (L.Filename, FileContents)
  -> Ex.Exceptional ErrorMsg Y.Ledger
parseFile (fn, (FileContents c)) =
  let p = fmap (addFileMetadata fn) CP.ledger
      fnStr = X.unpack . L.unFilename $ fn
  in case Parsec.parse p fnStr c of
    Left err -> Ex.throw (ErrorMsg . X.pack . show $ err)
    Right g -> return g

addFileMetadata ::
  L.Filename
  -> Y.Ledger
  -> Y.Ledger
addFileMetadata fn (Y.Ledger ls) =
  let eis = map toEiItem ls
      procTop s m =
        m { L.fileTransaction = Just (L.FileTransaction s)
          , L.filename = Just fn }
      procPstg s m =
        m { L.filePosting = Just (L.FilePosting s) }
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in Y.Ledger is'


addGlobalMetadata :: [Y.Ledger] -> Y.Ledger
addGlobalMetadata lss =
  let ls = concat . map Y.unLedger $ lss
      procTop s m =
        m { L.globalTransaction = Just (L.GlobalTransaction s) }
      procPstg s m =
        m { L.globalPosting = Just (L.GlobalPosting s) }
      eis = map toEiItem ls
      eis' = L.addSerialsToEithers procTop procPstg eis
      is' = map fromEiItem eis'
  in Y.Ledger is'

parse ::
  [(L.Filename, FileContents)]
  -> Ex.Exceptional ErrorMsg Y.Ledger
parse ps = fmap addGlobalMetadata $ mapM parseFile ps

data Other = OPrice L.PricePoint
             | OComment X.Text
             | OBlankLine
             deriving Show

type EiItem = Either Other L.Transaction

toEiItem :: Y.Item -> EiItem
toEiItem i = case i of
  Y.Transaction t -> Right t
  Y.PricePoint p -> Left (OPrice p)
  Y.Comment c -> Left (OComment c)
  Y.BlankLine -> Left OBlankLine

fromEiItem :: EiItem -> Y.Item
fromEiItem i = case i of
  Left l -> case l of
    OPrice p -> Y.PricePoint p
    OComment c -> Y.Comment c
    OBlankLine -> Y.BlankLine
  Right t -> Y.Transaction t

