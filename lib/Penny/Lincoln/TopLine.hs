module Penny.Lincoln.TopLine where

import Penny.Lincoln.Common
import Penny.Lincoln.Pieces
import Penny.Lincoln.Serial

data TopLineData = TopLineData
  { tlDateTime :: DateTime
  , tlMemo :: Memo
  , tlNumber :: Number
  , tlFlag :: Flag
  , tlPayee :: Payee
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

data TopLine = TopLine
  { tlData :: TopLineData
  , tlMeta :: Maybe TopLineMeta
  } deriving (Eq, Ord, Show)
