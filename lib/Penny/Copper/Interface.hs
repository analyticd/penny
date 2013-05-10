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
