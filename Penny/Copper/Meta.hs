module Penny.Copper.Meta (
  Column(Column, unColumn),
  PostingLine(PostingLine),
  TopMemoLine(TopMemoLine),
  TopLineLine(TopLineLine),
  PriceMeta(PriceMeta),
  M.Line(Line, unLine),
  M.Filename(Filename),
  PriceLine(PriceLine),
  M.Format(Format),
  TransactionMeta(TransactionMeta),
  PostingMeta(PostingMeta)) where

import qualified Penny.Lincoln.Meta as M

newtype Column = Column { unColumn :: Int }
                 deriving (Show, Eq, Ord)

newtype PriceLine = PriceLine M.Line
                    deriving Show

newtype PostingLine = PostingLine M.Line
                      deriving Show

newtype TopMemoLine = TopMemoLine M.Line
                      deriving Show

newtype TopLineLine = TopLineLine M.Line
                      deriving Show

data PriceMeta = PriceMeta PriceLine M.Format
                    deriving Show

instance M.HasMainLine PriceMeta where
  line (PriceMeta (PriceLine l) _) = l

instance M.HasFormat PriceMeta where
  format (PriceMeta _ f) = f

data PostingMeta = PostingMeta PostingLine (Maybe M.Format)
                   deriving Show

instance M.HasMainLine PostingMeta where
  line (PostingMeta (PostingLine l) _) = l

instance M.MayHaveFormat PostingMeta where
  maybeFormat (PostingMeta _ f) = f

data TransactionMeta =
  TransactionMeta (Maybe TopMemoLine) TopLineLine M.Filename
  deriving Show

instance M.HasMemoLine TransactionMeta where
  memoLine (TransactionMeta l _ _) = do
    (TopMemoLine lin) <- l
    return lin

instance M.HasFilename TransactionMeta where
  filename (TransactionMeta _ _ f) = f

instance M.HasMainLine TransactionMeta where
  line (TransactionMeta _ (TopLineLine l) _) = l
