{-# LANGUAGE DeriveGeneric #-}

module Penny.Copper.Interface where

import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Data.Text.Encoding as XE
import GHC.Generics (Generic)
import qualified Data.Binary as B

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

type ParsedItem
  =  Either ParsedTxn
    (Either L.PricePoint
    (Either Comment BlankLine))

type LedgerItem
  =  Either L.Transaction
    (Either L.PricePoint
    (Either Comment BlankLine))

type Parser
  = String
  -- ^ Filename of the file to be parsed
  -> IO (L.Filename, [ParsedItem])

-- | Changes a ledger item to remove metadata.
stripMeta
  :: LedgerItem
  ->  Either (L.TopLineCore, L.Ents L.PostingCore)
     (Either L.PricePoint
     (Either Comment BlankLine))
stripMeta =
  either (\t -> let (tl, es) = L.unTransaction t
                in Left (L.tlCore tl, fmap L.pdCore es)) Right

