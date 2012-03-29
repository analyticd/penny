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
module Penny.Cabin.Posts.Allocated (allocateCells, Fields(..)) where

import Control.Applicative(Applicative((<*>), pure), (<$>))
import qualified Data.Foldable as Fdbl
import qualified Data.Traversable as T
import qualified Data.Semigroup as Semi
import Data.Semigroup ((<>))
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Posts.Allocate as A
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Info as I
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Spacers as Spacers

data Fields a = Fields {
  payee :: a
  , account :: a
  } deriving (Eq, Show)

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
  
-- | Allocates cells. Returns a pair, with the first element being the
-- list of allocated cells, and the second indicating the width of the
-- cells, which will be greater than zero.
allocateCells ::
  Options.T a
  -> Spacers.T Int
  -> G.Fields (Maybe Int)
  -> O.ReportWidth
  -> [Info.T]
  -> Fields (Maybe ([R.Cell], Int))
allocateCells = undefined


-- | Takes allocations and whether a field appears. The returned
-- Fields has elements that are Nothing if the field does not appear
-- at all, or Just Int with the width if the field does appear. The
-- width does NOT include the width of the accompanying spacer.
-- Allocations are made after subtracting the width of the spacers.
allocate ::
  Fields (A.Allocation, Int)
  -> Fields Bool
  -> O.ReportWidth
  -> Fields Int
allocate = undefined

-- | Assign the allocations to a Fields.
allocations :: Options.T a -> Fields A.Allocation
allocations os = Fields {
  payee = O.payeeAllocation os
  , account = O.accountAllocation os }

-- | Taking into account the fields the user requested, and the
-- available space, calculate the fields that will actually appear.
-- If there is not enough space to accomodate ALL requested allocated
-- fields and their spacers, NONE of them will be shown.
appearingFields ::
  Fields Bool
  -> G.Fields (Maybe Int)
  -> Spacers.T Int
  -> O.ReportWidth
  -> Fields Bool
appearingFields flds grown spacers rw = let
  withSpacers = pairWithSpacer flds spacers
  demanded = spaceDemanded withSpacers
  maxAlloc = maxAllocatedWidth grown spacers rw
  in if demanded > maxAlloc then pure False else flds

-- | Calculate the space demanded by all allocated cells, including
-- their spacers. One field demands a single space, and whatever space
-- is set aside for its spacer. The first argument is a Fields holding
-- pairs, where the first element is whether the user asked for the
-- particular field to appear, and the second element is the width of
-- the accompanying spacer.
spaceDemanded ::
  Fields (Bool, Int)
  -> C.Width
spaceDemanded =
  C.Width
  . Semi.getSum
  . reduce
  . fmap Semi.Sum
  . fmap d
  where
    d (appears, width)
      | not appears = 0
      | width < 1 = 1
      | otherwise = width + 1

-- | Reduce a Fields holding a semigroup.
reduce :: Semi.Semigroup s => Fields s -> s
reduce f =
  payee f
  <> account f

allocatedFields :: Options.T a -> Fields Bool
allocatedFields o = let
  f = O.fields o in Fields {
    payee = F.payee f
    , account = F.account f }

-- | Pairs data from a Fields with its matching spacer field.
pairWithSpacer :: Fields a -> Spacers.T b -> Fields (a, b)
pairWithSpacer f s = Fields {
  payee = (payee f, S.payee s)
  , account = (account f, S.account s) }


-- | Calculate the maximum width to make available to allocated
-- cells. Apply to the width of all Grown fields, the width of all
-- spacers, and the width of the entire report. The width returned is
-- the width available both for the Payee and Allocation fields and
-- their associated spacers.
maxAllocatedWidth ::
  G.Fields (Maybe Int)
  -> Spacers.T Int
  -> O.ReportWidth
  -> C.Width
maxAllocatedWidth fs ss rw = C.Width $ max 0 diff where
  wGrown = G.grownWidth fs ss
  diff = O.unReportWidth rw - wGrown
