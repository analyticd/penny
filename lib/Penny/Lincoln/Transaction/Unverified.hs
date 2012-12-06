-- | Provides record types to hold the data that are in an unverified
-- transaction. Use these records along with the functions in
-- 'Penny.Lincoln.Transaction' to create Transactions. You can create
-- a Transaction only if the postings are balanced.
--
-- The functions that create transactions will fail at runtime if the
-- postings are not balanced (this is impossible to enforce at compile
-- time.) However, if you are creating a transaction in which both of
-- the amounts have the same commodity, for which all but one of the
-- entries will have the same DrCr, and in which one of the postings
-- has no entry at all (that is, its entry will be inferred), then it
-- is possible to create a transaction that is guaranteed to be
-- balanced. Use the RPosting and IPosting types for this purpose. (It
-- is necessary for all the postings except for the inferred one to
-- have the same DrCr because otherwise it would be possible to create
-- a transaction in which the inferred posting would have to have an
-- entry with a quantity of zero, which is impossible.
module Penny.Lincoln.Transaction.Unverified where

import qualified Penny.Lincoln.Bits as B

data TopLine = TopLine
  { tDateTime :: B.DateTime
  , tFlag     :: Maybe B.Flag
  , tNumber   :: Maybe B.Number
  , tPayee    :: Maybe B.Payee
  , tMemo     :: Maybe B.Memo
  , tTopLineLine :: Maybe B.TopLineLine
  , tTopMemoLine :: Maybe B.TopMemoLine
  , tFilename :: Maybe B.Filename
  , tGlobalTransaction :: Maybe B.GlobalTransaction
  , tFileTransaction :: Maybe B.FileTransaction
  } deriving (Eq, Show)

emptyTopLine :: B.DateTime -> TopLine
emptyTopLine d = TopLine
  { tDateTime = d
  , tFlag = Nothing
  , tNumber = Nothing
  , tPayee = Nothing
  , tMemo = Nothing
  , tTopLineLine = Nothing
  , tTopMemoLine = Nothing
  , tFilename = Nothing
  , tGlobalTransaction = Nothing
  , tFileTransaction = Nothing
  }

data Posting = Posting
  { pPayee   :: Maybe B.Payee
  , pNumber  :: Maybe B.Number
  , pFlag    :: Maybe B.Flag
  , pAccount :: B.Account
  , pTags    :: B.Tags
  , pEntry   :: Maybe B.Entry
  , pMemo    :: Maybe B.Memo
  , pSide :: Maybe B.Side
  , pSpaceBetween :: Maybe B.SpaceBetween
  , pPostingLine :: Maybe B.PostingLine
  , pGlobalPosting :: Maybe B.GlobalPosting
  , pFilePosting :: Maybe B.FilePosting
  } deriving (Eq, Show)

emptyPosting :: B.Account -> Posting
emptyPosting a = Posting
  { pPayee = Nothing
  , pNumber = Nothing
  , pFlag = Nothing
  , pAccount = a
  , pTags = B.Tags []
  , pEntry = Nothing
  , pMemo = Nothing
  , pSide = Nothing
  , pSpaceBetween = Nothing
  , pPostingLine = Nothing
  , pGlobalPosting = Nothing
  , pFilePosting = Nothing
  }

-- | A @restricted posting@ in which only the quantity is specified;
-- the commodity and DrCr are specified when the transaction is
-- created.
data RPosting = RPosting
  { rPayee   :: Maybe B.Payee
  , rNumber  :: Maybe B.Number
  , rFlag    :: Maybe B.Flag
  , rAccount :: B.Account
  , rTags    :: B.Tags
  , rQty     :: B.Qty
  , rMemo    :: Maybe B.Memo
  , rSide :: Maybe B.Side
  , rSpaceBetween :: Maybe B.SpaceBetween
  , rPostingLine :: Maybe B.PostingLine
  , rGlobalPosting :: Maybe B.GlobalPosting
  , rFilePosting :: Maybe B.FilePosting
  } deriving (Eq, Show)

emptyRPosting :: B.Account -> B.Qty -> RPosting
emptyRPosting a q = RPosting
  { rPayee = Nothing
  , rNumber = Nothing
  , rFlag = Nothing
  , rAccount = a
  , rTags = B.Tags []
  , rQty = q
  , rMemo = Nothing
  , rSide = Nothing
  , rSpaceBetween = Nothing
  , rPostingLine = Nothing
  , rGlobalPosting = Nothing
  , rFilePosting = Nothing
  }

-- | An @inferred posting@ in which no quantity is specified.
data IPosting = IPosting
  { iPayee   :: Maybe B.Payee
  , iNumber  :: Maybe B.Number
  , iFlag    :: Maybe B.Flag
  , iAccount :: B.Account
  , iTags    :: B.Tags
  , iMemo    :: Maybe B.Memo
  , iSide :: Maybe B.Side
  , iSpaceBetween :: Maybe B.SpaceBetween
  , iPostingLine :: Maybe B.PostingLine
  , iGlobalPosting :: Maybe B.GlobalPosting
  , iFilePosting :: Maybe B.FilePosting
  } deriving (Eq, Show)

emptyIPosting :: B.Account -> IPosting
emptyIPosting a = IPosting
  { iPayee = Nothing
  , iNumber = Nothing
  , iFlag = Nothing
  , iAccount = a
  , iTags = B.Tags []
  , iMemo = Nothing
  , iSide = Nothing
  , iSpaceBetween = Nothing
  , iPostingLine = Nothing
  , iGlobalPosting = Nothing
  , iFilePosting = Nothing
  }

