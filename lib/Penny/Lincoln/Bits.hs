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
  ) where


import Data.Monoid (mconcat)
import Penny.Lincoln.Bits.Open
import Penny.Lincoln.Bits.DateTime
import Penny.Lincoln.Bits.Qty
import Penny.Lincoln.Bits.Price

import qualified Penny.Lincoln.Bits.Open as O
import qualified Penny.Lincoln.Bits.DateTime as DT
import qualified Penny.Lincoln.Bits.Price as Pr
import qualified Penny.Lincoln.Equivalent as Ev
import Penny.Lincoln.Equivalent ((==~))

data PricePoint = PricePoint { dateTime :: DT.DateTime
                             , price :: Pr.Price
                             , ppSide :: Maybe O.Side
                             , ppSpaceBetween :: Maybe O.SpaceBetween
                             , priceLine :: Maybe O.PriceLine }
                  deriving (Eq, Show)


-- | PricePoint are equivalent if the dateTime and the Price are
-- equivalent. Other elements of the PricePoint are ignored.
instance Ev.Equivalent PricePoint where
  equivalent (PricePoint dx px _ _ _) (PricePoint dy py _ _ _) =
    dx ==~ dy && px ==~ py
  compareEv (PricePoint dx px _ _ _) (PricePoint dy py _ _ _) =
    mconcat [ Ev.compareEv dx dy
            , Ev.compareEv px py ]

-- | All the data that a TopLine might have.
data TopLineData = TopLineData
  { tlCore :: TopLineCore
  , tlFileMeta :: Maybe TopLineFileMeta
  , tlGlobal :: Maybe O.GlobalTransaction
  } deriving (Eq, Show)

emptyTopLineData :: DT.DateTime -> TopLineData
emptyTopLineData dt = TopLineData (emptyTopLineCore dt) Nothing Nothing

-- | Every TopLine has this data.
data TopLineCore = TopLineCore
  { tDateTime :: DT.DateTime
  , tNumber :: Maybe O.Number
  , tFlag :: Maybe O.Flag
  , tPayee :: Maybe O.Payee
  , tMemo :: Maybe O.Memo
  } deriving (Eq, Show)

-- | TopLineCore are equivalent if their dates are equivalent and if
-- everything else is equal.
instance Ev.Equivalent TopLineCore where
  equivalent x y =
    tDateTime x ==~ tDateTime y
    && tNumber x == tNumber y
    && tFlag x == tFlag y
    && tPayee x == tPayee y
    && tMemo x == tMemo y

  compareEv x y = mconcat
    [ Ev.compareEv (tDateTime x) (tDateTime y)
    , compare (tNumber x) (tNumber y)
    , compare (tFlag x) (tFlag y)
    , compare (tPayee x) (tPayee y)
    , compare (tMemo x) (tMemo y)
    ]

emptyTopLineCore :: DT.DateTime -> TopLineCore
emptyTopLineCore dt = TopLineCore dt Nothing Nothing Nothing Nothing

-- | TopLines from files have this metadata.
data TopLineFileMeta = TopLineFileMeta
  { tFilename :: O.Filename
  , tTopLineLine :: O.TopLineLine
  , tTopMemoLine :: Maybe O.TopMemoLine
  , tFileTransaction :: O.FileTransaction
  } deriving (Eq, Show)


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
  } deriving (Eq, Show)

-- | Two PostingCore are equivalent if the Tags are equivalent and the
-- other data is equal, exlucing the Side and the SpaceBetween, which are not considered at all.
instance Ev.Equivalent PostingCore where
  equivalent (PostingCore p1 n1 f1 a1 t1 m1 _ _)
             (PostingCore p2 n2 f2 a2 t2 m2 _ _)
    = p1 == p2 && n1 == n2 && f1 == f2
    && a1 == a2 && t1 ==~ t2 && m1 == m2

  compareEv (PostingCore p1 n1 f1 a1 t1 m1 _ _)
            (PostingCore p2 n2 f2 a2 t2 m2 _ _)
    = mconcat
        [ compare p1 p2
        , compare n1 n2
        , compare f1 f2
        , compare a1 a2
        , Ev.compareEv t1 t2
        , compare m1 m2
        ]

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

-- | Postings from files have this additional data.
data PostingFileMeta = PostingFileMeta
  { pPostingLine :: O.PostingLine
  , pFilePosting :: O.FilePosting
  } deriving (Eq, Show)


-- | All the data that a Posting might have.
data PostingData = PostingData
  { pdCore :: PostingCore
  , pdFileMeta :: Maybe PostingFileMeta
  , pdGlobal :: Maybe O.GlobalPosting
  } deriving (Eq, Show)

emptyPostingData :: O.Account -> PostingData
emptyPostingData ac = PostingData
  { pdCore = emptyPostingCore ac
  , pdFileMeta = Nothing
  , pdGlobal = Nothing
  }

