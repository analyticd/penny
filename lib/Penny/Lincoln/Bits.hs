{-# LANGUAGE DeriveGeneric, CPP #-}

-- | Essential data types used to make Transactions and Postings.
module Penny.Lincoln.Bits (
  -- * Accounts
  O.SubAccount(SubAccount, unSubAccount),
  O.Account(Account, unAccount),

  -- * Amounts
  O.Amount(Amount, qty, commodity),

  -- * Commodities
  O.Commodity(Commodity, unCommodity),

  -- * DateTime
  DT.TimeZoneOffset ( offsetToMins ),
  DT.minsToOffset,
  DT.noOffset,
  DT.Hours ( unHours ),
  DT.intToHours,
  DT.zeroHours,
  DT.Minutes ( unMinutes ),
  DT.intToMinutes,
  DT.zeroMinutes,
  DT.midnight,
  DT.Seconds ( unSeconds ),
  DT.intToSeconds,
  DT.zeroSeconds,
  DT.DateTime ( .. ),
  DT.dateTimeMidnightUTC,
  DT.toUTC,
  DT.toZonedTime,
  DT.fromZonedTime,
  DT.sameInstant,
  DT.showDateTime,

  -- * Debits and Credits
  O.DrCr(Debit, Credit),
  O.opposite,

  -- * Entries
  O.Entry(Entry, drCr, amount),

  -- * Flag
  O.Flag(Flag, unFlag),

  -- * Memos
  O.Memo(Memo, unMemo),

  -- * Number
  O.Number(Number, unNumber),

  -- * Payee
  O.Payee(Payee, unPayee),

  -- * Prices and price points
  Pr.From(From, unFrom), Pr.To(To, unTo),
  Pr.CountPerUnit(CountPerUnit, unCountPerUnit),
  Pr.Price(from, to, countPerUnit),
  Pr.convert, Pr.newPrice,
  PricePoint ( .. ),

  -- * Quantities
  Q.Qty, Q.NumberStr(..), Q.toQty, Q.mantissa, Q.places,
  Q.add, Q.mult, Q.difference, Q.equivalent, Q.newQty,
  Q.Mantissa, Q.Places,
  Q.Difference(Q.LeftBiggerBy, Q.RightBiggerBy, Q.Equal),
  Q.allocate,

  -- * Tags
  O.Tag(Tag, unTag),
  O.Tags(Tags, unTags)

  -- * Metadata
  , O.TopLineLine(..)
  , O.TopMemoLine(..)
  , O.Side(..)
  , O.SpaceBetween(..)
  , O.Filename(..)
  , O.PriceLine(..)
  , O.PostingLine(..)
  , O.GlobalPosting(..)
  , O.FilePosting(..)
  , O.GlobalTransaction(..)
  , O.FileTransaction(..)

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
  ) where


import qualified Penny.Lincoln.Bits.Open as O
import qualified Penny.Lincoln.Bits.DateTime as DT
import qualified Penny.Lincoln.Bits.Price as Pr
import qualified Penny.Lincoln.Bits.Qty as Q
import qualified Data.Binary as B
import GHC.Generics (Generic)

#ifdef test
import Control.Monad (liftM4, liftM2, liftM3, liftM5)
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck (Arbitrary, arbitrary)
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
#endif

