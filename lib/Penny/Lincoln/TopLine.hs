module Penny.Lincoln.TopLine where

import Penny.Lincoln.Common
import Penny.Lincoln.Pieces
import Penny.Lincoln.Serial

data TopLine = TopLine
  { tlDateTime :: DateTime
  , tlMemo :: Memo
  , tlNumber :: Number
  , tlPayee :: Payee
  , tlFlag :: Flag
  } deriving (Eq, Ord, Show)

data TopLineMeta = TopLineMeta
  { tlmTopMemo :: Line
  , tlmTopLine :: Line

  , tlmGlobalTransaction :: Serial
  -- ^ All transactions are numbered in order, beginning with the
  -- first transaction in the first file and ending with the last
  -- transaction in the last file.

  , tlmFileTransaction :: Serial
  -- ^ The transactions in each fhile are numbered in order.

  , tlmFilename :: Filename
  } deriving (Eq, Ord, Show)
