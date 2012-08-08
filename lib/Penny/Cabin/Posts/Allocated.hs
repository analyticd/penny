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
-- 2. Subtract from this sum the width of the Payee and Account
-- spacers:
--
-- a. Subtract the width of Payee spacer if it appears.
--
-- b. Subtract the width of the Account spacer if it appears.
--
-- 3. If the remaining width is 0 or less, do nothing. Return, but
-- indicate in return value that neither Payee nor Account is showing.
--
-- 4. Allocate the remaining width. If only Payee or Account appears,
-- it gets all the width; otherwise, allocate the widths. No special
-- arrangements are made if either field gets an allocation of 0.
--
-- 5. Fill cell contents. Return filled cells.
module Penny.Cabin.Posts.Allocated (
  payeeAndAcct
  , AllocatedOpts(..)
  , Fields(..)
  , SubAccountLength(..)
  ) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import Data.Maybe (catMaybes, isJust)
import Data.List (intersperse)
import qualified Data.Foldable as Fdbl
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import qualified Data.Text as X
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Posts.Allocate as A
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Meta as M
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.TextFormat as TF
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

data Fields a = Fields {
  payee :: a
  , account :: a
  } deriving (Eq, Show)

newtype SubAccountLength =
  SubAccountLength { unSubAccountLength :: Int }
  deriving Show

-- | All the information needed for allocated cells.
data AllocatedOpts = AllocatedOpts {
  fields :: Fields Bool
  , subAccountLength :: SubAccountLength
  , baseColors :: PC.BaseColors
  , payeeAllocation :: A.Allocation
  , accountAllocation :: A.Allocation
  , spacers :: S.Spacers Int
  , growerWidths :: G.Fields (Maybe Int)
  , reportWidth :: O.ReportWidth
  }

-- | Creates Payee and Account cells. The user must have requested the
-- cells. In addition, no cells are created if there is not enough
-- space for them in the report. Returns a Fields; each element of the
-- Fields is Nothing if no cells were created (either because the user
-- did not ask for them, or because there was no room) or Just cs i,
-- where cs is a list of all the cells, and i is the width of all the
-- cells.
payeeAndAcct ::
  AllocatedOpts
  -> [Box]
  -> Fields (Maybe ([R.ColumnSpec], Int))
payeeAndAcct as = allocateCells sl bc ws
  where
    sl = subAccountLength as
    bc = baseColors as
    ws = fieldWidth (fields as) (payeeAllocation as)
         (accountAllocation as) (spacers as)
         (growerWidths as) (reportWidth as)


-- | Allocates cells. Returns a pair, with the first element being the
-- list of allocated cells, and the second indicating the width of the
-- cells, which will be greater than zero.
allocateCells ::
  SubAccountLength
  -> PC.BaseColors
  -> Fields UnShrunkWidth
  -> [Box]
  -> Fields (Maybe ([R.ColumnSpec], Int))
allocateCells sl bc fs bs =
  let mkPayees i b = allocPayee i bc b
      mkAccts i b = allocAcct i sl bc b
      cellMakers = Fields mkPayees mkAccts
      mkCells (UnShrunkWidth width) maker =
        if width > 0
        then Just (map (maker width) bs)
        else Nothing
      unShrunkCells = mkCells <$> fs <*> cellMakers
  in fmap (fmap removeExtraSpace) unShrunkCells


-- | After first being allocated by allocPayee and allocAcct, cells
-- are as wide as the total space allocated. This function removes the
-- extra space, making all the cells as wide as the widest
-- cell. Returns the resized cells and the new width.
removeExtraSpace :: [R.ColumnSpec] -> ([R.ColumnSpec], Int)
removeExtraSpace cs = (trimmed, len) where
  len = Fdbl.foldl' f 0 cs where
    f acc c = max acc (Fdbl.foldl' g 0 (R.bits c)) where
      g inAcc chk = max inAcc (C.unWidth . C.chunkWidth $ chk)
  trimmed = map f cs where
    f c = c { R.width = C.Width len }

-- | The width of an on-screen field, after accounting for the width
-- of the entire report and the allocations but before shrinking.
newtype UnShrunkWidth = UnShrunkWidth Int
                      deriving Show

-- | Gets the width of the two allocated fields.
fieldWidth ::
  Fields Bool
  -> A.Allocation -- ^ Payee allocation
  -> A.Allocation -- ^ Accout allocation
  -> S.Spacers Int
  -> G.Fields (Maybe Int)
  -> O.ReportWidth
  -> Fields UnShrunkWidth
fieldWidth flds pa aa ss fs (O.ReportWidth rw) =
  let grownWidth = sumGrowersAndSpacers fs ss
      widthForCells = rw - grownWidth - allocSpacerWidth
      payeeSpacerWidth = if payee flds then abs (S.payee ss) else 0
      acctSpacerWidth = if account flds then abs (S.account ss) else 0
      allocSpacerWidth = payeeSpacerWidth + acctSpacerWidth
      allocs = (\bool alloc -> if bool then alloc else A.allocation 0)
               <$> flds
               <*> Fields pa aa
  in if widthForCells < 1
     then pure (UnShrunkWidth 0)
     else fmap UnShrunkWidth $ A.allocate allocs widthForCells


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

-- | Sums the contents of growing cells and their accompanying
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


allocPayee ::
  Int
  -- ^ Width that is permitted for this column
  -> PC.BaseColors
  -> Box
  -> R.ColumnSpec
allocPayee w bc i =
  let pb = L.boxPostFam i
      ts = PC.colors (M.visibleNum . L.boxMeta $ i) bc
      c = R.ColumnSpec j (C.Width w) ts sq
      j = R.LeftJustify
      sq = case Q.payee pb of
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
                C.chunk ts
                . X.unwords
                . Fdbl.toList
                $ seqTxts
          in fmap toBit wrapped
  in c


allocAcct ::
  Int
  -- ^ Width that is permitted for this column
  -> SubAccountLength
  -> PC.BaseColors
  -> Box
  -> R.ColumnSpec
allocAcct aw sl bc i =
  let pb = L.boxPostFam i
      ts = PC.colors (M.visibleNum . L.boxMeta $ i) bc
  in R.ColumnSpec R.LeftJustify (C.Width aw) ts $
     let target = TF.Target aw
         shortest = TF.Shortest . unSubAccountLength $ sl
         a = Q.account pb
         ws = TF.Words . Seq.fromList . HT.textList $ a
         (TF.Words shortened) = TF.shorten shortest target ws
     in [C.chunk ts
         . X.concat
         . intersperse (X.singleton ':')
         . Fdbl.toList
         $ shortened]

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
  
