{-# LANGUAGE DeriveGeneric #-}

module Penny.Copper.Interface where

import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Data.Text.Encoding as XE
import GHC.Generics (Generic)
import qualified Data.Binary as B
import qualified Penny.Steel.Sums as S

data ParsedTopLine = ParsedTopLine
  { ptlDateTime :: L.DateTime
  , ptlNumber :: Maybe L.Number
  , ptlFlag :: Maybe L.Flag
  , ptlPayee :: Maybe L.Payee
  , ptlMemo :: Maybe (L.Memo, L.TopMemoLine)
  , ptlTopLineLine :: L.TopLineLine
  } deriving (Show, Generic)

toTopLineCore :: ParsedTopLine -> L.TopLineCore
toTopLineCore (ParsedTopLine dt nu fl pa me _)
  = L.TopLineCore dt nu fl pa (fmap fst me)

instance B.Binary ParsedTopLine

type ParsedTxn = (ParsedTopLine , L.Ents (L.PostingCore, L.PostingLine))

data BlankLine = BlankLine
  deriving (Eq, Show, Generic)

instance B.Binary BlankLine

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

instance B.Binary Comment where
  get = fmap (Comment . XE.decodeUtf8) B.get
  put = B.put . XE.encodeUtf8 . unComment

type ParsedItem =
  S.S4 ParsedTxn L.PricePoint Comment BlankLine

type LedgerItem =
  S.S4 L.Transaction L.PricePoint Comment BlankLine

type Parser
  = String
  -- ^ Filename of the file to be parsed
  -> IO (L.Filename, [ParsedItem])

-- | Changes a ledger item to remove metadata.
stripMeta
  :: LedgerItem
  -> S.S4 (L.TopLineCore, L.Ents L.PostingCore)
          L.PricePoint
          Comment
          BlankLine
stripMeta = S.mapS4 f id id id where
  f t = let (tl, es) = L.unTransaction t
        in (L.tlCore tl, fmap L.pdCore es)

