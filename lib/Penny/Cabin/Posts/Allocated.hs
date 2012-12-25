-- | Calculates the allocated cells -- the Payee cell and the Account
-- cell. Here is the logic for this process:
--
-- 1. If neither Payee nor Account appears, do nothing.
--
-- 2. Obtain the width of the growing cells, including the
-- spacers. One of the spacers attached to a field might be omitted:
--
-- a. If the rightmost growing field is TotalQty, include all spacers.
--
-- b. If the rightmost growing field is to the left of Payee, include
-- all spacers.
--
-- c. If the rightmost growing field is to the right of Account but is
-- not TotalQty, omit its spacer.
--
-- 2. Obtain the width of the Payee and Account spacers. Include each
-- spacer if its corresponding field appears in the report.
--
-- 3. Subtract from the total report width the width of the the
-- growing cells and the width of the Payee and Account spacers. This
-- gives the total width available for the Payee and Account
-- fields. If there are not at least two columns available, return
-- without including the Payee and Account fields.
--
-- 4. Determine the total width that the Payee and Account fields
-- would obtain if they had all the space they could ever need. This
-- is the "requested width".
--
-- 5. Split up the available width for the Payee and Account fields
-- depending on which fields appear:
--
-- a. If only the one field appears, then it shall be as wide as the
-- total available width or the its requested width, whichever is
-- smaller.
--
-- b. If both fields appear, then calculate the allocated width for
-- each field. If either field's requested width is less than its
-- allocated width, then that field is only as wide as its requested
-- width. The other field is then as wide as (the sum of its allocated
-- width and the leftover width from the other field) or its requested
-- width, whichever is smaller. If neither field's requested width is
-- less than its allocated width, then each field gets ts allocated
-- width.
--
-- 6. Fill cell contents; return filled cells.

module Penny.Cabin.Posts.Allocated (
  payeeAndAcct
  , AllocatedOpts(..)
  , Fields(..)
  , SubAccountLength(..)
  , Alloc
  , alloc
  , unAlloc
  ) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import Control.Arrow (second)
import Data.Maybe (catMaybes, isJust)
import Data.List (intersperse)
import qualified Data.Foldable as Fdbl
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import qualified Data.Text as X
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Meta as M
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Types as Ty
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.TextFormat as TF
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Bits.Qty as Qty
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

data Fields a = Fields {
  payee :: a
  , account :: a
  } deriving (Eq, Show)

newtype SubAccountLength =
  SubAccountLength { unSubAccountLength :: Int }
  deriving Show

newtype Alloc = Alloc { unAlloc :: Int }
  deriving Show

alloc :: Int -> Alloc
alloc i =
  if i < 1
  then error $ "allocations must be greater than zero."
       ++ " supplied allocation: " ++ show i
  else Alloc i


-- | All the information needed for allocated cells.
data AllocatedOpts = AllocatedOpts
  { fields :: Fields Bool
  , subAccountLength :: SubAccountLength
  , allocations :: Fields Alloc
  , spacers :: S.Spacers Int
  , growerWidths :: G.Fields (Maybe Int)
  , reportWidth :: Ty.ReportWidth
  }

-- | Creates Payee and Account cells. The user must have requested the
-- cells. In addition, no cells are created if there is not enough
-- space for them in the report. Returns a Fields; each element of the
-- Fields is Nothing if no cells were created (either because the user
-- did not ask for them, or because there was no room) or Just cs i,
-- where cs is a list of all the cells, and i is the width of all the
-- cells.
payeeAndAcct
  :: AllocatedOpts
  -> [Box]
  -> Fields (Maybe ([R.ColumnSpec], Int))
payeeAndAcct ao bs =
  let allBuilders =
        T.traverse (builders (subAccountLength ao)) bs
      availWidth = availableWidthForAllocs (growerWidths ao)
                   (spacers ao) (fields ao) (reportWidth ao)
      finals = divideAvailableWidth availWidth (fields ao)
               (allocations ao)
               (fmap maximum . fmap (fmap fst) $ allBuilders)
  in fmap (fmap (second unFinal))
     . buildSpecs finals
     . fmap (fmap snd)
     $ allBuilders


payeeAndAccountSpacerWidth
  :: Fields Bool
  -> S.Spacers Int
  -> Int
payeeAndAccountSpacerWidth flds ss = pye + act
  where
    pye = if payee flds then abs (S.payee ss) else 0
    act = if account flds then abs (S.account ss) else 0

newtype AvailableWidth = AvailableWidth Int
        deriving (Eq, Ord, Show)

availableWidthForAllocs
  :: G.Fields (Maybe Int)
  -> S.Spacers Int
  -> Fields Bool
  -> Ty.ReportWidth
  -> AvailableWidth
availableWidthForAllocs growers ss flds (Ty.ReportWidth w) =
  AvailableWidth $ max 0 diff
  where
    tot = sumGrowersAndSpacers growers ss
          + payeeAndAccountSpacerWidth flds ss
    diff = w - tot

-- | Sums spacers for growing cells. This function is intended for use
-- only by the functions that allocate cells for the report, so it
-- assumes that either the Payee or the Account field is showing. Sums
-- all spacers, UNLESS the rightmost field is from PostingDrCr to
-- TotalCmdty, in which case the rightmost spacer is omitted. Apply to
-- the second element of the tuple returned by growCells (which
-- reflects which fields actually have width) and to the accompanying
-- Spacers.
sumSpacers ::
  G.Fields (Maybe a)
  -> S.Spacers Int
  -> Int
sumSpacers fs =
  sum
  . map fst
  . appearingSpacers
  . catMaybes
  . Fdbl.toList
  . fmap toWidth
  . pairedWithSpacers fs


-- | Takes a triple:
--
-- * The first element is Just _ if the field appears in the report;
-- Nothing if not
--
-- * The second element is Maybe Int for the width of the spacer
-- (TotalQty has no spacer, so it will be Nothing)
--
-- * The third element is the EFields tag
--
-- Returns Nothing if the field does not appear in the report. Returns
-- Just a pair if the field does appear in the report, where the first
-- element is the width of the spacer, and the second element is the
-- EFields tag.
toWidth :: (Maybe a, Maybe Int, t) -> Maybe (Int, t)
toWidth (maybeShowing, maybeWidth, tag) =
  if isJust maybeShowing
  then case maybeWidth of
    Just w -> Just (w, tag)
    Nothing -> Just (0, tag)
  else Nothing


-- | Given a list of all spacers that are attached to the fields that
-- are present in a report, return a list of the spacers that will
-- actually appear in the report. The rightmost spacer does not appear
-- if it is to the right of Account (unless there is a TotalQty field,
-- in which case, all spacers appear because TotalQty has no spacer.)
appearingSpacers :: [(Int, G.EFields)] -> [(Int, G.EFields)]
appearingSpacers ss = case ss of
  [] -> []
  l -> case snd $ last l of
    G.ETotalQty -> l
    t -> if t > G.ENumber
         then init l
         else l

-- | Applied to two arguments: first, a Fields, and second, a
-- Spacers. Combines each Field with its corresponding Spacer and with
-- the GFields, which indicates each particular field.
pairedWithSpacers ::
  G.Fields a
  -> S.Spacers b
  -> G.Fields (a, Maybe b, G.EFields)
pairedWithSpacers f s =
  (\(a, b) c -> (a, b, c))
  <$> G.pairWithSpacer f s
  <*> G.eFields

-- | Sums the widths of growing cells and their accompanying
-- spacers; makes the adjustments described in sumSpacers.
sumGrowersAndSpacers ::
  G.Fields (Maybe Int)
  -> S.Spacers Int
  -> Int
sumGrowersAndSpacers fs ss = spcrs + flds where
  spcrs = sumSpacers fs ss
  flds = Fdbl.foldr f 0 fs where
    f maybeI acc = case maybeI of
      Nothing -> acc
      Just i -> acc + i

newtype Request = Request { unRequest :: Int }
        deriving (Eq, Ord, Show)

newtype Final = Final { unFinal :: Int }
        deriving (Eq, Ord, Show)


buildSpecs
  :: Fields (Maybe Final)
  -> Fields ([Final -> R.ColumnSpec])
  -> Fields (Maybe ([R.ColumnSpec], Final))
buildSpecs finals bs = f <$> finals <*> bs
  where
    f mayFinal gs = case mayFinal of
      Nothing -> Nothing
      Just fin -> Just ((gs <*> pure fin), fin)


-- | Divide the total available width between the two fields.
divideAvailableWidth
  :: AvailableWidth
  -> Fields Bool
  -> Fields Alloc
  -> Fields Request
  -> Fields (Maybe Final)
divideAvailableWidth (AvailableWidth aw) appear allocs rws = Fields pye act
  where
    minFinal i1 i2 =
      let m = min i1 i2
      in if m > 0 then Just . Final $ m else Nothing
    pairAtLeast i1 i2 = (atLeast i1, atLeast i2)
      where atLeast i = if i > 0 then Just . Final $ i else Nothing
    reqP = unRequest . payee $ rws
    reqA = unRequest . account $ rws
    (pye, act) = case (payee appear, account appear) of
      (False, False) -> (Nothing, Nothing)
      (True, False) -> (minFinal reqP aw, Nothing)
      (False, True) -> (Nothing, minFinal reqA aw)
      (True, True) ->
        let votes = [unAlloc . payee $ allocs, unAlloc . account $ allocs]
            allocRslt = Qty.largestRemainderMethod (fromIntegral aw)
                        (map fromIntegral votes)
            (allocP, allocA) = case allocRslt of
              x:y:[] -> (fromIntegral x, fromIntegral y)
              _ -> error "divideAvailableWidth error"
        in case (allocP > reqP, allocA > reqA) of
            (True, True) -> pairAtLeast reqP reqA
            (True, False) ->
              pairAtLeast reqP $ (min (allocA + (allocP - reqP))) reqA
            (False, True) ->
              pairAtLeast (min reqP (allocP + (allocA - reqA))) reqA
            (False, False) -> pairAtLeast allocP allocA


builders
  :: SubAccountLength
  -> Box
  -> Fields (Request, Final -> R.ColumnSpec)
builders sl b = Fields (buildPayee b) (buildAcct sl b)

buildPayee
  :: Box
  -> (Request, Final -> R.ColumnSpec)
  -- ^ Returns a tuple. The first element is the maximum width that
  -- this cell needs to display its value perfectly. The second
  -- element is a function that, when applied to an actual width,
  -- returns a ColumnSpec.

buildPayee i = (maxW, mkSpec)
  where
    pb = L.boxPostFam i
    eo = E.fromVisibleNum . M.visibleNum . L.boxMeta $ i
    j = R.LeftJustify
    ps = (E.Other, eo)
    mayPye = Q.payee pb
    maxW = Request $ maybe 0 (X.length . HT.text) mayPye
    mkSpec (Final w) = R.ColumnSpec j (C.Width w) ps sq
      where
        sq = case mayPye of
          Nothing -> []
          Just pye ->
            let wrapped =
                  Fdbl.toList
                  . TF.unLines
                  . TF.wordWrap w
                  . TF.txtWords
                  . HT.text
                  $ pye
                toBit (TF.Words seqTxts) =
                  E.PreChunk E.Other eo
                  . X.unwords
                  . Fdbl.toList
                  $ seqTxts
            in fmap toBit wrapped


buildAcct ::
  SubAccountLength
  -> Box
  -> (Request, Final -> R.ColumnSpec)
  -- ^ Returns a tuple. The first element is the maximum width that
  -- this cell needs to display its value perfectly. The second
  -- element is a function that, when applied to an actual width,
  -- returns a ColumnSpec.

buildAcct sl i = (maxW, mkSpec)
  where
    pb = L.boxPostFam i
    eo = E.fromVisibleNum . M.visibleNum . L.boxMeta $ i
    ps = (E.Other, eo)
    aList = L.unAccount . Q.account $ pb
    maxW = Request
           $ (sum . map (X.length . L.unSubAccount) $ aList)
           + max 0 (length aList - 1)
    mkSpec (Final aw) = R.ColumnSpec R.LeftJustify (C.Width aw) ps sq
      where
        target = TF.Target aw
        shortest = TF.Shortest . unSubAccountLength $ sl
        ws = TF.Words . Seq.fromList . map L.unSubAccount $ aList
        (TF.Words shortened) = TF.shorten shortest target ws
        sq = [ E.PreChunk E.Other eo
               . X.concat
               . intersperse (X.singleton ':')
               . Fdbl.toList
               $ shortened ]

instance Functor Fields where
  fmap f i = Fields {
    payee = f (payee i)
    , account = f (account i) }

instance Applicative Fields where
  pure a = Fields a a
  ff <*> fa = Fields {
    payee = payee ff (payee fa)
    , account = account ff (account fa) }

instance Fdbl.Foldable Fields where
  foldr f z flds =
    f (payee flds) (f (account flds) z)

instance T.Traversable Fields where
  traverse f flds =
    Fields <$> f (payee flds) <*> f (account flds)

