-- | Fills the bottom rows, which contain the tags, memo, and
-- filename. These rows are formatted as follows:
--
-- * If the columns for TotalDrCr, TotalCmdty, and TotalQty are all
-- present, AND if there are at least TWO other columns present, then
-- there will be a hanging indent. The bottom rows will begin at the
-- SECOND column and end with the last column to the left of
-- TotalDrCr. In this case, each bottom row will have three cells: one
-- padding on the left, one main content, and one padding on the
-- right.
--
-- * Otherwise, if there are NO columns in the top row, these rows
-- will take the entire width of the report. Each bottom row will have
-- one cell.
--
-- * Otherwise, the bottom rows are as wide as all the top cells
-- combined. Each bottom row will have one cell.

module Penny.Cabin.Posts.BottomRows (
  BottomOpts(..),
  bottomRows, Fields(..), TopRowCells(..), mergeWithSpacers,
  topRowCells) where

import Control.Applicative((<$>), Applicative(pure,  (<*>)))
import qualified Data.Foldable as Fdbl
import Control.Monad (guard)
import Data.List (intersperse, find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Monoid (mappend, mempty, First(First, getFirst))
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Traversable as T
import qualified System.Console.Rainbow as Rb
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.TextFormat as TF
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Types as Ty
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.Queries as Q

data BottomOpts = BottomOpts
  { growingWidths :: G.Fields (Maybe Int)
  , allocatedWidths :: A.Fields (Maybe Int)
  , fields :: F.Fields Bool
  , reportWidth :: Ty.ReportWidth
  , spacers :: S.Spacers Int
  }

bottomRows
  :: E.Changers
  -> BottomOpts
  -> [(M.PostMeta, L.Posting)]
  -> Fields (Maybe [[Rb.Chunk]])
bottomRows ch os bs = makeRows bs pcs where
  pcs = infoProcessors ch topSpecs (reportWidth os) wanted
  wanted = requestedMakers ch (fields os)
  topSpecs = topCellSpecs (growingWidths os) (allocatedWidths os)
             (spacers os)


data Fields a = Fields {
  tags :: a
  , memo :: a
  , filename :: a
  } deriving (Show, Eq)

instance Fdbl.Foldable Fields where
  foldr f z d =
    f (tags d)
    (f (memo d)
     (f (filename d) z))

instance Functor Fields where
  fmap f (Fields t m fn) =
    Fields (f t) (f m) (f fn)

instance Applicative Fields where
  pure a = Fields a a a
  ff <*> fa = Fields {
    tags = (tags ff) (tags fa)
    , memo = (memo ff) (memo fa)
    , filename = (filename ff) (filename fa)
    }

bottomRowsFields :: F.Fields a -> Fields a
bottomRowsFields f = Fields {
  tags = F.tags f
  , memo = F.memo f
  , filename = F.filename f }


data Hanging a = Hanging {
  leftPad :: a
  , mainCell :: a
  , rightPad :: a
  } deriving (Show, Eq)


newtype SpacerWidth = SpacerWidth Int deriving (Show, Eq)
newtype ContentWidth = ContentWidth Int deriving (Show, Eq)


hanging
  :: E.Changers
  -> [TopCellSpec]
  -> Maybe (((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec))
            -> (M.PostMeta, L.Posting) -> [Rb.Chunk])
hanging ch specs = hangingWidths specs
                >>= return . hangingInfoProcessor ch

hangingInfoProcessor
  :: E.Changers
  -> Hanging Int
  -> ((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec))
  -> (M.PostMeta, L.Posting)
  -> [Rb.Chunk]
hangingInfoProcessor ch widths mkr info = row where
  row = R.row ch [left, mid, right]
  (ts, mid) = mkr info (mainCell widths)
  mkPad w = R.ColumnSpec R.LeftJustify (R.Width w) ts []
  left = mkPad (leftPad widths)
  right = mkPad (rightPad widths)

widthOfTopColumns
  :: E.Changers
  -> [TopCellSpec]
  -> Maybe (((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec))
            -> (M.PostMeta, L.Posting) -> [Rb.Chunk])
widthOfTopColumns ch ts =
  if null ts
  then Nothing
  else Just $ makeSpecificWidth ch w where
    w = Fdbl.foldl' f 0 ts
    f acc (_, maySpcWidth, (ContentWidth cw)) =
      acc + cw + maybe 0 (\(SpacerWidth sw) -> sw) maySpcWidth


widthOfReport
  :: E.Changers
  -> Ty.ReportWidth
  -> ((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec))
  -> (M.PostMeta, L.Posting)
  -> [Rb.Chunk]
widthOfReport ch (Ty.ReportWidth rw) fn info =
  makeSpecificWidth ch rw fn info

chooseProcessor
  :: E.Changers
  -> [TopCellSpec]
  -> Ty.ReportWidth
  -> ((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec))
  -> (M.PostMeta, L.Posting)
  -> [Rb.Chunk]
chooseProcessor ch specs rw fn = let
  firstTwo = First (hanging ch specs)
             `mappend` First (widthOfTopColumns ch specs)
  in case getFirst firstTwo of
    Nothing -> widthOfReport ch rw fn
    Just r -> r fn

infoProcessors
  :: E.Changers
  -> [TopCellSpec]
  -> Ty.ReportWidth
  -> Fields (Maybe ((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec)))
  -> Fields (Maybe ((M.PostMeta, L.Posting) -> [Rb.Chunk]))
infoProcessors ch specs rw flds = let
  chooser = chooseProcessor ch specs rw
  mkProcessor mayFn = case mayFn of
    Nothing -> Nothing
    Just fn -> Just $ chooser fn
  in mkProcessor <$> flds


makeRows ::
  [(M.PostMeta, L.Posting)]
  -> Fields (Maybe ((M.PostMeta, L.Posting) -> [Rb.Chunk]))
  -> Fields (Maybe [[Rb.Chunk]])
makeRows is flds = let
  mkRow fn = map fn is
  in fmap (fmap mkRow) flds


-- | Calculates column widths for a Hanging report. If it cannot
-- calculate the widths (because these cells do not support hanging),
-- returns Nothing.
hangingWidths :: [TopCellSpec]
                 -> Maybe (Hanging Int)
hangingWidths ls = do
  let len = length ls
  guard (len > 4)
  let matchColumn x (c, _, _) = x == c
  totDrCr <- find (matchColumn ETotalDrCr) ls
  totCmdty <- find (matchColumn ETotalCmdty) ls
  totQty <- find (matchColumn ETotalQty) ls
  let (first:middle) = take (len - 3) ls
  mid <- NE.nonEmpty middle
  return $ calcHangingWidths first mid (totDrCr, totCmdty, totQty)

type TopCellSpec = (ETopRowCells, Maybe SpacerWidth, ContentWidth)

-- | Given the first column in the top row, at least one middle
-- column, and the last three columns, calculate the width of the
-- three columns in the hanging report.
calcHangingWidths ::
  TopCellSpec
  -> NE.NonEmpty TopCellSpec
  -> (TopCellSpec, TopCellSpec, TopCellSpec)
  -> Hanging Int
calcHangingWidths l m r = Hanging left middle right where
  calcWidth (_, maybeSp, (ContentWidth c)) =
    c + maybe 0 (\(SpacerWidth w) -> abs w) maybeSp
  left = calcWidth l
  middle = Fdbl.foldl' f 0 m where
    f acc c = acc + calcWidth c
  (totDrCr, totCmdty, totQty) = r
  right = calcWidth totDrCr + calcWidth totCmdty
          + calcWidth totQty


topCellSpecs :: G.Fields (Maybe Int)
                -> A.Fields (Maybe Int)
                -> S.Spacers Int
                -> [TopCellSpec]
topCellSpecs gFlds aFlds spcs = let
  allFlds = topRowCells gFlds aFlds
  cws = fmap (fmap ContentWidth) allFlds
  merged = mergeWithSpacers cws spcs
  tripler e (cw, maybeSpc) = (e, (fmap SpacerWidth maybeSpc), cw)
  list = Fdbl.toList $ tripler <$> eTopRowCells <*> merged
  toMaybe (e, maybeS, maybeC) = case maybeC of
    Nothing -> Nothing
    Just c -> Just (e, maybeS, c)
  in catMaybes (map toMaybe list)


-- | Merges a TopRowCells with a Spacers. Returns Maybes because
-- totalQty has no spacer.
mergeWithSpacers ::
  TopRowCells a
  -> S.Spacers b
  -> TopRowCells (a, Maybe b)
mergeWithSpacers t s = TopRowCells {
  globalTransaction = (globalTransaction t, Just (S.globalTransaction s))
  , revGlobalTransaction = (revGlobalTransaction t, Just (S.revGlobalTransaction s))
  , globalPosting = (globalPosting t, Just (S.globalPosting s))
  , revGlobalPosting = (revGlobalPosting t, Just (S.revGlobalPosting s))
  , fileTransaction = (fileTransaction t, Just (S.fileTransaction s))
  , revFileTransaction = (revFileTransaction t, Just (S.revFileTransaction s))
  , filePosting = (filePosting t, Just (S.filePosting s))
  , revFilePosting = (revFilePosting t, Just (S.revFilePosting s))
  , filtered = (filtered t, Just (S.filtered s))
  , revFiltered = (revFiltered t, Just (S.revFiltered s))
  , sorted = (sorted t, Just (S.sorted s))
  , revSorted = (revSorted t, Just (S.revSorted s))
  , visible = (visible t, Just (S.visible s))
  , revVisible = (revVisible t, Just (S.revVisible s))
  , lineNum = (lineNum t, Just (S.lineNum s))
  , date = (date t, Just (S.date s))
  , flag = (flag t, Just (S.flag s))
  , number = (number t, Just (S.number s))
  , payee = (payee t, Just (S.payee s))
  , account = (account t, Just (S.account s))
  , postingDrCr = (postingDrCr t, Just (S.postingDrCr s))
  , postingCmdty = (postingCmdty t, Just (S.postingCmdty s))
  , postingQty = (postingQty t, Just (S.postingQty s))
  , totalDrCr = (totalDrCr t, Just (S.totalDrCr s))
  , totalCmdty = (totalCmdty t, Just (S.totalCmdty s))
  , totalQty = (totalQty t, Nothing) }


-- | Applied to a function that, when applied to the width of a cell,
-- returns a cell filled with data, returns a Row with that cell.
makeSpecificWidth
  :: E.Changers -> Int -> ((M.PostMeta, L.Posting) -> Int -> (a, R.ColumnSpec))
  -> (M.PostMeta, L.Posting) -> [Rb.Chunk]
makeSpecificWidth ch w f i = R.row ch [c] where
  (_, c) = f i w


type Maker
  = E.Changers
  -> (M.PostMeta, L.Posting)
  -> Int
  -> ((E.Label, E.EvenOdd), R.ColumnSpec)

makers :: Fields Maker
makers = Fields tagsCell memoCell filenameCell

-- | Applied to an Options, indicating which reports the user wants,
-- returns a Fields (Maybe Maker) with a Maker in each respective
-- field that the user wants to see.
requestedMakers
  :: E.Changers
  -> F.Fields Bool
  -> Fields (Maybe ((M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec)))
requestedMakers ch allFlds =
  let flds = bottomRowsFields allFlds
      filler b mkr = if b then Just $ mkr ch else Nothing
  in filler <$> flds <*> makers

tagsCell
  :: E.Changers
  -> (M.PostMeta, L.Posting)
  -> Int
  -> ((E.Label, E.EvenOdd), R.ColumnSpec)
tagsCell ch info w = (ts, cell) where
  vn = M.visibleNum . fst $ info
  cell = R.ColumnSpec R.LeftJustify (R.Width w) ts cs
  eo = E.fromVisibleNum vn
  ts = (E.Other, eo)
  cs =
    Fdbl.toList
    . fmap toBit
    . TF.unLines
    . TF.wordWrap w
    . TF.Words
    . Seq.fromList
    . map (X.cons '*')
    . HT.textList
    . Q.tags
    . snd
    $ info
  md = E.getEvenOddLabelValue E.Other eo ch
  toBit (TF.Words ws) = md . Rb.plain $ t where
    t = X.concat . intersperse (X.singleton ' ') . Fdbl.toList $ ws


memoBits
  :: E.Changers -> (E.Label, E.EvenOdd) -> L.Memo -> R.Width -> [Rb.Chunk]
memoBits ch (lbl, eo) m (R.Width w) = cs where
  cs = Fdbl.toList
       . fmap toBit
       . TF.unLines
       . TF.wordWrap w
       . TF.Words
       . Seq.fromList
       . X.words
       . X.intercalate (X.singleton ' ')
       . L.unMemo
       $ m
  md = E.getEvenOddLabelValue lbl eo ch
  toBit (TF.Words ws) = md . Rb.plain $ (X.unwords . Fdbl.toList $ ws)


memoCell
  :: E.Changers -> (M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec)
memoCell ch info width = (ts, cell) where
  w = R.Width width
  vn = M.visibleNum . fst $ info
  eo = E.fromVisibleNum vn
  ts = (E.Other, eo)
  cell = R.ColumnSpec R.LeftJustify w ts cs
  mayPm = Q.postingMemo . snd $ info
  mayTm = Q.transactionMemo . snd $ info
  cs = case (mayPm, mayTm) of
    (Nothing, Nothing) -> mempty
    (Nothing, Just tm) -> memoBits ch ts tm w
    (Just pm, Nothing) -> memoBits ch ts pm w
    (Just pm, Just tm) -> memoBits ch ts pm w `mappend` memoBits ch ts tm w


filenameCell
  :: E.Changers -> (M.PostMeta, L.Posting) -> Int -> ((E.Label, E.EvenOdd), R.ColumnSpec)
filenameCell ch info width = (ts, cell) where
  w = R.Width width
  vn = M.visibleNum . fst $ info
  eo = E.fromVisibleNum vn
  ts = (E.Other, eo)
  cell = R.ColumnSpec R.LeftJustify w ts cs
  md = E.getEvenOddLabelValue E.Other eo ch
  toBit n = md . Rb.plain
            . X.drop (max 0 (X.length n - width)) $ n
  cs = case Q.filename . snd $ info of
    Nothing -> []
    Just fn -> [toBit . L.unFilename $ fn]


data TopRowCells a = TopRowCells
  { globalTransaction    :: a
  , revGlobalTransaction :: a
  , globalPosting        :: a
  , revGlobalPosting     :: a
  , fileTransaction      :: a
  , revFileTransaction   :: a
  , filePosting          :: a
  , revFilePosting       :: a
  , filtered             :: a
  , revFiltered          :: a
  , sorted               :: a
  , revSorted            :: a
  , visible              :: a
  , revVisible           :: a
  , lineNum              :: a
    -- ^ The line number from the posting's metadata
  , date                 :: a
  , flag                 :: a
  , number               :: a
  , payee                :: a
  , account              :: a
  , postingDrCr          :: a
  , postingCmdty         :: a
  , postingQty           :: a
  , totalDrCr            :: a
  , totalCmdty           :: a
  , totalQty             :: a }
  deriving (Show, Eq)

topRowCells :: G.Fields a -> A.Fields a -> TopRowCells a
topRowCells g a = TopRowCells
  { globalTransaction    = G.globalTransaction g
  , revGlobalTransaction = G.revGlobalTransaction g
  , globalPosting        = G.globalPosting g
  , revGlobalPosting     = G.revGlobalPosting g
  , fileTransaction      = G.fileTransaction g
  , revFileTransaction   = G.revFileTransaction g
  , filePosting          = G.filePosting g
  , revFilePosting       = G.revFilePosting g
  , filtered             = G.filtered g
  , revFiltered          = G.revFiltered g
  , sorted               = G.sorted g
  , revSorted            = G.revSorted g
  , visible              = G.visible g
  , revVisible           = G.revVisible g
  , lineNum              = G.lineNum g
  , date                 = G.date g
  , flag                 = G.flag g
  , number               = G.number g
  , payee                = A.payee a
  , account              = A.account a
  , postingDrCr          = G.postingDrCr g
  , postingCmdty         = G.postingCmdty g
  , postingQty           = G.postingQty g
  , totalDrCr            = G.totalDrCr g
  , totalCmdty           = G.totalCmdty g
  , totalQty             = G.totalQty g }


data ETopRowCells =
  EGlobalTransaction
  | ERevGlobalTransaction
  | EGlobalPosting
  | ERevGlobalPosting
  | EFileTransaction
  | ERevFileTransaction
  | EFilePosting
  | ERevFilePosting
  | EFiltered
  | ERevFiltered
  | ESorted
  | ERevSorted
  | EVisible
  | ERevVisible
  | ELineNum
  | EDate
  | EFlag
  | ENumber
  | EPayee
  | EAccount
  | EPostingDrCr
  | EPostingCmdty
  | EPostingQty
  | ETotalDrCr
  | ETotalCmdty
  | ETotalQty
  deriving (Show, Eq, Enum)

eTopRowCells :: TopRowCells ETopRowCells
eTopRowCells = TopRowCells
  { globalTransaction    = EGlobalTransaction
  , revGlobalTransaction = ERevGlobalTransaction
  , globalPosting        = EGlobalPosting
  , revGlobalPosting     = ERevGlobalPosting
  , fileTransaction      = EFileTransaction
  , revFileTransaction   = ERevFileTransaction
  , filePosting          = EFilePosting
  , revFilePosting       = ERevFilePosting
  , filtered             = EFiltered
  , revFiltered          = ERevFiltered
  , sorted               = ESorted
  , revSorted            = ERevSorted
  , visible              = EVisible
  , revVisible           = ERevVisible
  , lineNum              = ELineNum
  , date                 = EDate
  , flag                 = EFlag
  , number               = ENumber
  , payee                = EPayee
  , account              = EAccount
  , postingDrCr          = EPostingDrCr
  , postingCmdty         = EPostingCmdty
  , postingQty           = EPostingQty
  , totalDrCr            = ETotalDrCr
  , totalCmdty           = ETotalCmdty
  , totalQty             = ETotalQty }

instance Functor TopRowCells where
  fmap f t = TopRowCells
    { globalTransaction    = f (globalTransaction    t)
    , revGlobalTransaction = f (revGlobalTransaction t)
    , globalPosting        = f (globalPosting        t)
    , revGlobalPosting     = f (revGlobalPosting     t)
    , fileTransaction      = f (fileTransaction      t)
    , revFileTransaction   = f (revFileTransaction   t)
    , filePosting          = f (filePosting          t)
    , revFilePosting       = f (revFilePosting       t)
    , filtered             = f (filtered             t)
    , revFiltered          = f (revFiltered          t)
    , sorted               = f (sorted               t)
    , revSorted            = f (revSorted            t)
    , visible              = f (visible              t)
    , revVisible           = f (revVisible           t)
    , lineNum              = f (lineNum              t)
    , date                 = f (date                 t)
    , flag                 = f (flag                 t)
    , number               = f (number               t)
    , payee                = f (payee                t)
    , account              = f (account              t)
    , postingDrCr          = f (postingDrCr          t)
    , postingCmdty         = f (postingCmdty         t)
    , postingQty           = f (postingQty           t)
    , totalDrCr            = f (totalDrCr            t)
    , totalCmdty           = f (totalCmdty           t)
    , totalQty             = f (totalQty             t) }

instance Applicative TopRowCells where
  pure a = TopRowCells
    { globalTransaction    = a
    , revGlobalTransaction = a
    , globalPosting        = a
    , revGlobalPosting     = a
    , fileTransaction      = a
    , revFileTransaction   = a
    , filePosting          = a
    , revFilePosting       = a
    , filtered             = a
    , revFiltered          = a
    , sorted               = a
    , revSorted            = a
    , visible              = a
    , revVisible           = a
    , lineNum              = a
    , date                 = a
    , flag                 = a
    , number               = a
    , payee                = a
    , account              = a
    , postingDrCr          = a
    , postingCmdty         = a
    , postingQty           = a
    , totalDrCr            = a
    , totalCmdty           = a
    , totalQty             = a }

  ff <*> fa = TopRowCells
    { globalTransaction    = globalTransaction    ff (globalTransaction    fa)
    , revGlobalTransaction = revGlobalTransaction ff (revGlobalTransaction fa)
    , globalPosting        = globalPosting        ff (globalPosting        fa)
    , revGlobalPosting     = revGlobalPosting     ff (revGlobalPosting     fa)
    , fileTransaction      = fileTransaction      ff (fileTransaction      fa)
    , revFileTransaction   = revFileTransaction   ff (revFileTransaction   fa)
    , filePosting          = filePosting          ff (filePosting          fa)
    , revFilePosting       = revFilePosting       ff (revFilePosting       fa)
    , filtered             = filtered             ff (filtered             fa)
    , revFiltered          = revFiltered          ff (revFiltered          fa)
    , sorted               = sorted               ff (sorted               fa)
    , revSorted            = revSorted            ff (revSorted            fa)
    , visible              = visible              ff (visible              fa)
    , revVisible           = revVisible           ff (revVisible           fa)
    , lineNum              = lineNum              ff (lineNum              fa)
    , date                 = date                 ff (date                 fa)
    , flag                 = flag                 ff (flag                 fa)
    , number               = number               ff (number               fa)
    , payee                = payee                ff (payee                fa)
    , account              = account              ff (account              fa)
    , postingDrCr          = postingDrCr          ff (postingDrCr          fa)
    , postingCmdty         = postingCmdty         ff (postingCmdty         fa)
    , postingQty           = postingQty           ff (postingQty           fa)
    , totalDrCr            = totalDrCr            ff (totalDrCr            fa)
    , totalCmdty           = totalCmdty           ff (totalCmdty           fa)
    , totalQty             = totalQty             ff (totalQty             fa) }

instance Fdbl.Foldable TopRowCells where
  foldr f z o =
    f (globalTransaction o)
    (f (revGlobalTransaction o)
     (f (globalPosting o)
      (f (revGlobalPosting o)
       (f (fileTransaction o)
        (f (revFileTransaction o)
         (f (filePosting o)
          (f (revFilePosting o)
           (f (filtered o)
            (f (revFiltered o)
             (f (sorted o)
              (f (revSorted o)
               (f (visible o)
                (f (revVisible o)
                 (f (lineNum o)
                  (f (date o)
                   (f (flag o)
                    (f (number o)
                     (f (payee o)
                      (f (account o)
                       (f (postingDrCr o)
                        (f (postingCmdty o)
                         (f (postingQty o)
                          (f (totalDrCr o)
                           (f (totalCmdty o)
                            (f (totalQty o) z)))))))))))))))))))))))))

instance T.Traversable TopRowCells where
  traverse f t =
    TopRowCells
    <$> f (globalTransaction t)
    <*> f (revGlobalTransaction t)
    <*> f (globalPosting t)
    <*> f (revGlobalPosting t)
    <*> f (fileTransaction t)
    <*> f (revFileTransaction t)
    <*> f (filePosting t)
    <*> f (revFilePosting t)
    <*> f (filtered t)
    <*> f (revFiltered t)
    <*> f (sorted t)
    <*> f (revSorted t)
    <*> f (visible t)
    <*> f (revVisible t)
    <*> f (lineNum t)
    <*> f (date t)
    <*> f (flag t)
    <*> f (number t)
    <*> f (payee t)
    <*> f (account t)
    <*> f (postingDrCr t)
    <*> f (postingCmdty t)
    <*> f (postingQty t)
    <*> f (totalDrCr t)
    <*> f (totalCmdty t)
    <*> f (totalQty t)

