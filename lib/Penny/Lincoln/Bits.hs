{-# LANGUAGE DeriveGeneric, CPP #-}

-- | Essential data types used to make Transactions and Postings.
module Penny.Lincoln.Bits
  ( module Penny.Lincoln.Bits.Open
  , module Penny.Lincoln.Bits.DateTime
  , module Penny.Lincoln.Bits.Price
  , module Penny.Lincoln.Bits.Qty
  , PricePoint ( .. )

  -- * Aggregates
  , TopLineCore(..)
  , emptyTopLineCore
  , TopLineFileMeta(..)
  , TopLineData(..)
  , emptyTopLineData
  , PostingCore(..)
  , emptyPostingCore
  , PostingFileMeta(..)
  , PostingData(..)
  , emptyPostingData

#ifdef test
  , tests
#endif
  ) where


import Penny.Lincoln.Bits.Open
import Penny.Lincoln.Bits.DateTime
import Penny.Lincoln.Bits.Price
import Penny.Lincoln.Bits.Qty hiding (tests)

import qualified Penny.Lincoln.Bits.Open as O
import qualified Penny.Lincoln.Bits.DateTime as DT
import qualified Penny.Lincoln.Bits.Price as Pr
import qualified Data.Binary as B
import GHC.Generics (Generic)

#ifdef test
import Control.Monad (liftM4, liftM2, liftM3, liftM5)
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.Framework (Test, testGroup)
import qualified Penny.Lincoln.Bits.Qty as Q
#endif

data PricePoint = PricePoint { dateTime :: DT.DateTime
                             , price :: Pr.Price
                             , ppSide :: Maybe O.Side
                             , ppSpaceBetween :: Maybe O.SpaceBetween
                             , priceLine :: Maybe O.PriceLine }
                  deriving (Eq, Show, Generic)

instance B.Binary PricePoint

-- | All the data that a TopLine might have.
data TopLineData = TopLineData
  { tlCore :: TopLineCore
  , tlFileMeta :: Maybe TopLineFileMeta
  , tlGlobal :: Maybe O.GlobalTransaction
  } deriving (Eq, Show, Generic)

emptyTopLineData :: DT.DateTime -> TopLineData
emptyTopLineData dt = TopLineData (emptyTopLineCore dt) Nothing Nothing

instance B.Binary TopLineData

#ifdef test
instance Arbitrary TopLineData where
  arbitrary = liftM3 TopLineData arbitrary arbitrary arbitrary
#endif

-- | Every TopLine has this data.
data TopLineCore = TopLineCore
  { tDateTime :: DT.DateTime
  , tNumber :: Maybe O.Number
  , tFlag :: Maybe O.Flag
  , tPayee :: Maybe O.Payee
  , tMemo :: Maybe O.Memo
  } deriving (Eq, Show, Generic)

emptyTopLineCore :: DT.DateTime -> TopLineCore
emptyTopLineCore dt = TopLineCore dt Nothing Nothing Nothing Nothing

instance B.Binary TopLineCore

#ifdef test
instance Arbitrary TopLineCore where
  arbitrary = liftM5 TopLineCore arbitrary arbitrary arbitrary
              arbitrary arbitrary
#endif

-- | TopLines from files have this metadata.
data TopLineFileMeta = TopLineFileMeta
  { tFilename :: O.Filename
  , tTopLineLine :: O.TopLineLine
  , tTopMemoLine :: Maybe O.TopMemoLine
  , tFileTransaction :: O.FileTransaction
  } deriving (Eq, Show, Generic)

instance B.Binary TopLineFileMeta


#ifdef test
instance Arbitrary TopLineFileMeta where
  arbitrary = liftM4 TopLineFileMeta arbitrary arbitrary
              arbitrary arbitrary
#endif

-- | All Postings have this data.
data PostingCore = PostingCore
  { pPayee :: Maybe O.Payee
  , pNumber :: Maybe O.Number
  , pFlag :: Maybe O.Flag
  , pAccount :: O.Account
  , pTags :: O.Tags
  , pMemo :: Maybe O.Memo
  , pSide :: Maybe O.Side
  , pSpaceBetween :: Maybe O.SpaceBetween
  } deriving (Eq, Show, Generic)

emptyPostingCore :: O.Account -> PostingCore
emptyPostingCore ac = PostingCore
  { pPayee = Nothing
  , pNumber = Nothing
  , pFlag = Nothing
  , pAccount = ac
  , pTags = O.Tags []
  , pMemo = Nothing
  , pSide = Nothing
  , pSpaceBetween = Nothing
  }

instance B.Binary PostingCore

#ifdef test
instance Arbitrary PostingCore where
  arbitrary = PostingCore <$> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
              <*> arbitrary
#endif

-- | Postings from files have this additional data.
data PostingFileMeta = PostingFileMeta
  { pPostingLine :: O.PostingLine
  , pFilePosting :: O.FilePosting
  } deriving (Eq, Show, Generic)

instance B.Binary PostingFileMeta

#ifdef test
instance Arbitrary PostingFileMeta where
  arbitrary = liftM2 PostingFileMeta arbitrary arbitrary
#endif

-- | All the data that a Posting might have.
data PostingData = PostingData
  { pdCore :: PostingCore
  , pdFileMeta :: Maybe PostingFileMeta
  , pdGlobal :: Maybe O.GlobalPosting
  } deriving (Eq, Show, Generic)

emptyPostingData :: O.Account -> PostingData
emptyPostingData ac = PostingData
  { pdCore = emptyPostingCore ac
  , pdFileMeta = Nothing
  , pdGlobal = Nothing
  }

instance B.Binary PostingData

#ifdef test
instance Arbitrary PostingData where
  arbitrary = liftM3 PostingData arbitrary arbitrary arbitrary

tests :: Test
tests = testGroup "Penny.Lincoln.Bits"
  [ Q.tests
  ]

#endif

