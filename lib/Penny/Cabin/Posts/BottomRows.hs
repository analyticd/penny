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

module Penny.Cabin.Posts.BottomRows where

import Control.Applicative((<$>), (<*>))
import qualified Data.Foldable as Fdbl
import Data.List (intersperse)
import Data.Monoid (mappend, mempty)
import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Traversable as T
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.TextFormat as TF
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.Colors as PC
import qualified Penny.Cabin.Posts.Fields as Fields
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Spacers as Spacers
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Lincoln.Queries as Q


data Fields a = Fields {
  tags :: a
  , memo :: a
  , filename :: a
  } deriving (Show, Eq)


bottomRowsFields :: Fields.T a -> Fields a
bottomRowsFields f = Fields {
  tags = F.tags f
  , memo = F.memo f
  , filename = F.filename f }


data BottomLayout =
  HangingIndent
  | WidthOfTopColumns
  | WidthOfReport
  deriving (Show, Eq)


-- | Examines the top row to determine what sort of layout is
-- necessary for the bottom cells. Apply to a TopRowCells Bool, which
-- indicates which of the top row cells are actually in the report
-- (not the ones the user requested; some of the ones the user
-- requested might not be in the report because they had no data, or
-- because there was no space.)
layout :: TopRowCells (Bool, ETopRowCells) -> BottomLayout
layout tr
  | null ls = WidthOfReport
  | canHang ls = HangingIndent
  | otherwise = WidthOfTopColumns
    where
      ls = map snd . filter fst . Fdbl.toList $ tr


canHang :: [ETopRowCells] -> Bool
canHang ls
  | length ls > 5 && ETotalDrCr `elem` ls && ETotalCmdty `elem` ls
    && ETotalQty `elem` ls = True
  | otherwise = False


data Hanging a = Hanging {
  leftPad :: a
  , mainCell :: a
  , rightPad :: a
  } deriving (Show, Eq)


newtype SpacerWidth = SpacerWidth Int deriving (Show, Eq)
newtype ContentWidth = ContentWidth Int deriving (Show, Eq)


-- | Calculates column widths for a Hanging report. If it cannot
-- calculate the widths (because these cells do not support hanging),
-- returns Nothing.
hangingWidths :: [(ETopRowCells, Maybe SpacerWidth, ContentWidth)]
                 -> Maybe (Hanging Int)
hangingWidths = undefined


-- | Merges a TopRowCells with a Spacers. Returns Maybes because
-- totalQty has no spacer.
mergeWithSpacers ::
  TopRowCells a
  -> Spacers.T b
  -> TopRowCells (a, Maybe b)
mergeWithSpacers t s = TopRowCells {
  postingNum = (postingNum t, Just (S.postingNum s)) 
  , visibleNum = (visibleNum t, Just (S.visibleNum s))
  , revPostingNum = (revPostingNum t, Just (S.revPostingNum s))
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


-- | Applied to a Hanging describing the widths of the columns, and to
-- a function that, when applied to the width of the middle cell,
-- returns the TextSpec for the padding cells and returns the middle
-- cell itself, returns a Row with all three cells.
makeHanging ::
  Hanging Int
  -> (Int -> (C.TextSpec, R.Cell))
  -> R.Row
makeHanging (Hanging lw mw rw) f = row where
  (ts, m) = f mw
  padder w = R.Cell R.LeftJustify (C.Width w) ts Seq.empty
  (l, r) = (padder lw, padder rw)
  row = l `R.prependCell` (m `R.prependCell`
        (r `R.prependCell` R.emptyRow))
  

-- | Applied to a function that, when applied to the width of a cell,
-- returns a cell filled with data, returns a Row with that cell.
makeSpecificWidth :: Int -> (Int -> (a, R.Cell)) -> R.Row
makeSpecificWidth w f = c `R.prependCell` R.emptyRow where
  (_, c) = f w


tagsCell :: Options.T a -> Info.T -> Int -> (C.TextSpec, R.Cell)
tagsCell os info w = (ts, cell) where
  vn = Info.visibleNum info
  cell = R.Cell R.LeftJustify (C.Width w) ts cs
  ts = PC.colors vn (O.baseColors os)
  cs =
    fmap toChunk
    . TF.unLines
    . TF.wordWrap w
    . TF.Words
    . Seq.fromList
    . map (X.cons '*')
    . HT.textList
    . Q.tags
    . Info.postingBox
    $ info
  toChunk (TF.Words ws) = C.chunk ts t where
    t = X.concat . intersperse (X.singleton ' ') . Fdbl.toList $ ws


memoChunks :: C.TextSpec -> L.Memo -> C.Width -> Seq.Seq C.Chunk
memoChunks ts m (C.Width w) = cs where
  cs = fmap toChunk
       . TF.unLines
       . TF.wordWrap w
       . TF.Words
       . Seq.fromList
       . X.words
       . HT.text
       . HT.Delimited (X.singleton ' ')
       . HT.textList
       $ m
  toChunk (TF.Words ws) = C.chunk ts (X.unwords . Fdbl.toList $ ws)


memoCell :: Options.T a -> Info.T -> Int -> (C.TextSpec, R.Cell)
memoCell os info width = (ts, cell) where
  w = C.Width width
  vn = Info.visibleNum info
  cell = R.Cell R.LeftJustify w ts cs
  ts = PC.colors vn (O.baseColors os)
  pm = Q.postingMemo . Info.postingBox $ info
  tm = Q.transactionMemo . Info.postingBox $ info
  nullMemo (L.Memo m) = null m
  cs = case (nullMemo pm, nullMemo tm) of
    (True, True) -> mempty
    (False, True) -> memoChunks ts pm w
    (True, False) -> memoChunks ts tm w
    (False, False) -> memoChunks ts pm w `mappend` memoChunks ts tm w
  

filenameCell :: Options.T a -> Info.T -> Int -> (C.TextSpec, R.Cell)
filenameCell os info width = (ts, cell) where
  w = C.Width width
  vn = Info.visibleNum info
  cell = R.Cell R.LeftJustify w ts cs
  toChunk n = C.chunk ts
              . X.drop (max 0 (C.unWidth w - X.length n)) $ n
  cs = case Q.filename . Info.postingBox $ info of
    Nothing -> Seq.empty
    Just fn -> Seq.singleton . toChunk . L.unFilename $ fn
  ts = PC.colors vn (O.baseColors os)



data TopRowCells a = TopRowCells {
  postingNum :: a
  , visibleNum :: a
  , revPostingNum :: a
  , lineNum :: a
  , date :: a
  , flag :: a
  , number :: a
  , payee :: a
  , account :: a
  , postingDrCr :: a
  , postingCmdty :: a
  , postingQty :: a
  , totalDrCr :: a
  , totalCmdty :: a
  , totalQty :: a }
  deriving (Show, Eq)

topRowCells :: G.Fields a -> A.Fields a -> TopRowCells a
topRowCells g a = TopRowCells {
  postingNum = G.postingNum g
  , visibleNum = G.visibleNum g
  , revPostingNum = G.revPostingNum g
  , lineNum = G.lineNum g
  , date = G.date g
  , flag = G.flag g
  , number = G.number g
  , payee = A.payee a
  , account = A.account a
  , postingDrCr = G.postingDrCr g
  , postingCmdty = G.postingCmdty g
  , postingQty = G.postingQty g
  , totalDrCr = G.totalDrCr g
  , totalCmdty = G.totalCmdty g
  , totalQty = G.totalQty g }


data ETopRowCells =
  EPostingNum
  | EVisibleNum
  | ERevPostingNum
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
eTopRowCells = TopRowCells {
    postingNum = EPostingNum
  , visibleNum = EVisibleNum
  , revPostingNum = ERevPostingNum
  , lineNum = ELineNum
  , date = EDate
  , flag = EFlag
  , number = ENumber
  , payee = EPayee
  , account = EAccount
  , postingDrCr = EPostingDrCr
  , postingCmdty = EPostingCmdty
  , postingQty = EPostingQty
  , totalDrCr = ETotalDrCr
  , totalCmdty = ETotalCmdty
  , totalQty = ETotalQty }

instance Functor TopRowCells where
  fmap f t = TopRowCells {
    postingNum = f (postingNum t)
    , visibleNum = f (visibleNum t)
    , revPostingNum = f (revPostingNum t)
    , lineNum = f (lineNum t)
    , date = f (date t)
    , flag = f (flag t)
    , number = f (number t)
    , payee = f (payee t)
    , account = f (account t)
    , postingDrCr = f (postingDrCr t)
    , postingCmdty = f (postingCmdty t)
    , postingQty = f (postingQty t)
    , totalDrCr = f (totalDrCr t)
    , totalCmdty = f (totalCmdty t)
    , totalQty = f (totalQty t) }

instance Fdbl.Foldable TopRowCells where
  foldr f z o =
    f (postingNum o)
    (f (visibleNum o)
     (f (revPostingNum o)
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
                 (f (totalQty o) z))))))))))))))

instance T.Traversable TopRowCells where
  traverse f t =
    TopRowCells
    <$> f (postingNum t)
    <*> f (visibleNum t)
    <*> f (revPostingNum t)
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

