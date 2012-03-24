module Penny.Cabin.Postings.AllocInspector where

import qualified Data.Array as A
import qualified Data.Table as Tb
import qualified Data.Foldable as Fdbl
import Data.Map ((!))
import qualified Data.Map as M
import Data.Monoid (mappend, mempty)
import qualified Data.Text as X

import qualified Penny.Cabin.Allocate as Alo
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Colors as C
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe R.Cell)

type Address = (Col, Row)

-- | Returns a map indicating the width of the payee column and the
-- width of the account column. If a column does not appear in the
-- report at all, its width is zero. If a column does appear, then its
-- width is the SMALLER of the widest cell in the column or the
-- allocated width of the column.
allocInspector :: O.Options a -> Arr -> M.Map Col C.Width
allocInspector o a = M.fromList ls where
  ls = [(Adr.Payee, pw), (Adr.Account, aw)]
  (pwAlo, awAlo) = calcMaxAloWidths o a
  pw = case pwAlo of
    Nothing -> C.Width 0
    Just alo -> let
      maxText = maxPayeeReservedWidth a
      (PayeeWidth result) = min maxText alo
      in C.Width result
  aw = case awAlo of
    Nothing -> C.Width 0
    Just alo -> let
      maxText = maxAccountReservedWidth a
      (AccountWidth result) = min maxText alo
      in C.Width result

newtype PayeeWidth = PayeeWidth { unPayeeWidth :: Int }
                     deriving (Show, Eq, Ord)

newtype AccountWidth = AccountWidth { unAccountWidth :: Int }
                       deriving (Show, Eq, Ord)

-- | Examines all payee cells to determine which has the widest text
-- width.
maxPayeeReservedWidth :: Arr -> PayeeWidth
maxPayeeReservedWidth a = Fdbl.foldl' folder (PayeeWidth 0) clm where
  clm = map toInfo . filter p . A.assocs $ a where
    toInfo (_, (info, _)) = info
    p ((c, (_, r)), _) = c == Adr.Payee && r == Adr.Top
  folder maxSoFar info = max maxSoFar w where
    w = case Q.payee . T.postingBox $ info of
      Nothing -> (PayeeWidth 0)
      (Just pye) -> PayeeWidth . X.length . HT.text $ pye


-- | Examines all account cells to determine which has the widest text
-- width.
maxAccountReservedWidth :: Arr -> AccountWidth
maxAccountReservedWidth a = Fdbl.foldl' folder (AccountWidth 0) clm where
  clm = map toInfo . filter p . A.assocs $ a where
    toInfo (_, (info, _)) = info
    p ((c, (_, r)), _) = c == Adr.Account && r == Adr.Top
  folder maxSoFar info = max maxSoFar (AccountWidth w) where
    w = X.length . HT.text . HT.Delimited (X.singleton ':')
        . HT.textList . Q.account . T.postingBox $ info


-- | Composes functions to calculate the maximum allocated widths from
-- an array.
calcMaxAloWidths ::
  O.Options a
  -> Arr
  -> (Maybe PayeeWidth, Maybe AccountWidth)
calcMaxAloWidths o = maxAloWidths . allAloWidths o

-- | Returns a list of all VisibleNum in an array.
allVisibleNum :: Arr -> [T.VisibleNum]
allVisibleNum a = A.range (minVn, maxVn) where
  bnds = A.bounds a
  minVn = fst . snd . fst $ bnds
  maxVn = fst . snd . snd $ bnds

-- | Gets a list of all PayeeWidth and AccountWidth for the whole
-- array. Assumes that each column appears in the report.
allAloWidths ::
  O.Options a
  -> Arr
  -> [(Maybe PayeeWidth, Maybe AccountWidth)]
allAloWidths os arr = map (aloWidths os arr) (allVisibleNum arr)

-- | Determines the maximum allocated widths.
maxAloWidths :: [(Maybe PayeeWidth, Maybe AccountWidth)]
                -> (Maybe PayeeWidth, Maybe AccountWidth)
maxAloWidths = Fdbl.foldl' f (Nothing, Nothing) where
  f (pwT, awT) (pw, aw) = (pwM, awM) where
    pwM = case (pw, pwT) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just p) -> Just p
      (Just p, Nothing) -> Just p
      (Just p1, Just p2) -> Just (max p1 p2)
    awM = case (aw, awT) of
      (Nothing, Nothing) -> Nothing
      (Nothing, Just a) -> Just a
      (Just a, Nothing) -> Just a
      (Just p1, Just p2) -> Just (max p1 p2)
                

-- | Examines the array to determine the allocated width of the Payee
-- column and the Account column for a particular row.
aloWidths :: O.Options a
          -> Arr
          -> T.VisibleNum
          -> (Maybe PayeeWidth, Maybe AccountWidth)
aloWidths os arr vn = (pw, aw) where
  allocs = M.fromList [ ('p', O.payeeAllocation os)
                      , ('a', O.accountAllocation os) ]
  widthTop = topRowWidth vn arr
  widthMax = maxAllocatedWidth widthTop (O.width os)
  ws = Alo.allocate allocs
           (fromIntegral . unMaxAllocated $ widthMax)
  pay = PayeeWidth . fromIntegral . (! 'p') $ ws
  acct = AccountWidth . fromIntegral . (! 'a') $ ws
  payeeAll = PayeeWidth . fromIntegral . unMaxAllocated $ widthMax
  acctAll = AccountWidth . fromIntegral . unMaxAllocated $ widthMax
  flds = O.fields os
  (pw, aw) = case (F.payee flds, F.account flds) of
    (True, True) -> (Just pay, Just acct)
    (False, False) -> (Nothing, Nothing)
    (True, False) -> (Just payeeAll, Nothing)
    (False, True) -> (Nothing, Just acctAll)

topRowWidth :: T.VisibleNum -> Arr -> C.Width
topRowWidth vn a = Fdbl.foldr mappend mempty r where
  r = fmap toWidth . Tb.OneDim . Tb.row a $ (vn, Adr.Top)
  toWidth (_, cell) = case cell of
    Just c -> R.width c
    _ -> mempty

newtype MaxAllocated = MaxAllocated { unMaxAllocated :: Int }
                       deriving (Eq, Ord, Show)

maxAllocatedWidth :: C.Width -> O.ReportWidth -> MaxAllocated
maxAllocatedWidth (C.Width rowW) (O.ReportWidth reportW) =
  MaxAllocated $ max 0 (reportW - rowW)

{-

allocationClaim ::
  O.Options a
  -> G.AllocationClaim Col Row
allocationClaim _ _ _ (_, Just c) = G.AcCell c

allocationClaim opts _ (col, (_, tr)) (p, _) = case (col, tr) of
  (Adr.Payee, Adr.Top) -> let
    w = case Q.payee . T.postingBox $ p of
      Nothing -> 0
      (Just pye) -> X.length . HT.text $ pye
    in if F.payee . O.fields $ opts
       then G.AcWidth w
       else G.AcCell R.zeroCell
  (Adr.Account, Adr.Top) -> let
    w = X.length . HT.text . HT.Delimited (X.singleton ':')
        . HT.textList . Q.account . T.postingBox $ p
    in if F.account . O.fields $ opts
       then G.AcWidth w
       else G.AcCell R.zeroCell
  (Adr.Multi, row) -> case row of
    Adr.Top -> error "allocationClaim: error 1: should not happen"
    _ -> G.AcOverrunning
  _ -> error "allocationClaim: error 2: should not happen"
-}
