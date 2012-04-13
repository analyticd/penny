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
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.TextFormat as TF
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Colors as PC
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

bottomRows ::
  G.Fields (Maybe Int)
  -> A.Fields (Maybe Int)
  -> Options.T
  -> [Info.T]
  -> Fields (Maybe [[C.Bit]])
bottomRows gf af os is = makeRows is pcs where
  pcs = infoProcessors topSpecs rw wanted
  wanted = requestedMakers os
  topSpecs = topCellSpecs gf af (O.spacers os)
  rw = O.width os


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

bottomRowsFields :: Fields.T a -> Fields a
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


hanging ::
  [TopCellSpec]
  -> Maybe ((Info.T -> Int -> (C.TextSpec, R.ColumnSpec))
            -> Info.T -> [C.Bit])
hanging specs = hangingWidths specs
                >>= return . hangingInfoProcessor

hangingInfoProcessor ::
  Hanging Int
  -> (Info.T -> Int -> (C.TextSpec, R.ColumnSpec))
  -> Info.T
  -> [C.Bit]
hangingInfoProcessor widths mkr info = row where
  row = R.row [left, mid, right]
  (ts, mid) = mkr info (mainCell widths)
  mkPad w = R.ColumnSpec R.LeftJustify (C.Width w) ts []
  left = mkPad (leftPad widths)
  right = mkPad (rightPad widths)

widthOfTopColumns ::
  [TopCellSpec]
  -> Maybe ((Info.T -> Int -> (C.TextSpec, R.ColumnSpec))
            -> Info.T -> [C.Bit])
widthOfTopColumns ts =
  if null ts
  then Nothing
  else Just $ makeSpecificWidth w where
    w = Fdbl.foldl' f 0 ts
    f acc (_, maySpcWidth, (ContentWidth cw)) =
      acc + cw + maybe 0 (\(SpacerWidth sw) -> sw) maySpcWidth


widthOfReport ::
  O.ReportWidth
  -> (Info.T -> Int -> (C.TextSpec, R.ColumnSpec))
  -> Info.T
  -> [C.Bit]
widthOfReport (O.ReportWidth rw) fn info =
  makeSpecificWidth rw fn info

chooseProcessor ::
  [TopCellSpec]
  -> O.ReportWidth
  -> (Info.T -> Int -> (C.TextSpec, R.ColumnSpec))
  -> Info.T
  -> [C.Bit]
chooseProcessor specs rw fn = let
  firstTwo = First (hanging specs)
             `mappend` First (widthOfTopColumns specs)
  in case getFirst firstTwo of
    Nothing -> widthOfReport rw fn
    Just r -> r fn

infoProcessors ::
  [TopCellSpec]
  -> O.ReportWidth
  -> Fields (Maybe (Info.T -> Int -> (C.TextSpec, R.ColumnSpec)))
  -> Fields (Maybe (Info.T -> [C.Bit]))
infoProcessors specs rw flds = let
  chooser = chooseProcessor specs rw
  mkProcessor mayFn = case mayFn of
    Nothing -> Nothing
    Just fn -> Just $ chooser fn
  in mkProcessor <$> flds


makeRows ::
  [Info.T]
  -> Fields (Maybe (Info.T -> [C.Bit]))
  -> Fields (Maybe [[C.Bit]])
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
                -> Spacers.T Int
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


-- | Applied to a function that, when applied to the width of a cell,
-- returns a cell filled with data, returns a Row with that cell.
makeSpecificWidth :: Int -> (Info.T -> Int -> (a, R.ColumnSpec))
                     -> Info.T -> [C.Bit]
makeSpecificWidth w f i = R.row [c] where
  (_, c) = f i w


type Maker = Options.T -> Info.T -> Int -> (C.TextSpec, R.ColumnSpec)

makers :: Fields Maker
makers = Fields tagsCell memoCell filenameCell

-- | Applied to an Options, indicating which reports the user wants,
-- returns a Fields (Maybe Maker) with a Maker in each respective
-- field that the user wants to see.
requestedMakers ::
  Options.T
  -> Fields (Maybe (Info.T -> Int -> (C.TextSpec, R.ColumnSpec)))
requestedMakers os = let
  flds = bottomRowsFields (O.fields os)
  filler b mkr = if b then Just $ mkr os else Nothing
  in filler <$> flds <*> makers

tagsCell :: Options.T -> Info.T -> Int -> (C.TextSpec, R.ColumnSpec)
tagsCell os info w = (ts, cell) where
  vn = Info.visibleNum info
  cell = R.ColumnSpec R.LeftJustify (C.Width w) ts cs
  ts = PC.colors vn (O.baseColors os)
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
    . Info.postingBox
    $ info
  toBit (TF.Words ws) = C.bit ts t where
    t = X.concat . intersperse (X.singleton ' ') . Fdbl.toList $ ws


memoBits :: C.TextSpec -> L.Memo -> C.Width -> [C.Bit]
memoBits ts m (C.Width w) = cs where
  cs = Fdbl.toList
       . fmap toBit
       . TF.unLines
       . TF.wordWrap w
       . TF.Words
       . Seq.fromList
       . X.words
       . HT.text
       . HT.Delimited (X.singleton ' ')
       . HT.textList
       $ m
  toBit (TF.Words ws) = C.bit ts (X.unwords . Fdbl.toList $ ws)


memoCell :: Options.T -> Info.T -> Int -> (C.TextSpec, R.ColumnSpec)
memoCell os info width = (ts, cell) where
  w = C.Width width
  vn = Info.visibleNum info
  cell = R.ColumnSpec R.LeftJustify w ts cs
  ts = PC.colors vn (O.baseColors os)
  pm = Q.postingMemo . Info.postingBox $ info
  tm = Q.transactionMemo . Info.postingBox $ info
  nullMemo (L.Memo m) = null m
  cs = case (nullMemo pm, nullMemo tm) of
    (True, True) -> mempty
    (False, True) -> memoBits ts pm w
    (True, False) -> memoBits ts tm w
    (False, False) -> memoBits ts pm w `mappend` memoBits ts tm w
  

filenameCell :: Options.T -> Info.T -> Int -> (C.TextSpec, R.ColumnSpec)
filenameCell os info width = (ts, cell) where
  w = C.Width width
  vn = Info.visibleNum info
  cell = R.ColumnSpec R.LeftJustify w ts cs
  toBit n = C.bit ts
            . X.drop (max 0 (X.length n - width)) $ n
  cs = case Q.filename . Info.postingBox $ info of
    Nothing -> []
    Just fn -> [toBit . L.unFilename $ fn]
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

instance Applicative TopRowCells where
  pure a = TopRowCells {
    postingNum = a
    , visibleNum = a
    , revPostingNum = a
    , lineNum = a
    , date = a
    , flag = a
    , number = a
    , payee = a
    , account = a
    , postingDrCr = a
    , postingCmdty = a
    , postingQty = a
    , totalDrCr = a
    , totalCmdty = a
    , totalQty = a }

  ff <*> fa = TopRowCells {
    postingNum = (postingNum ff) (postingNum fa)
    , visibleNum = (visibleNum ff) (visibleNum fa)
    , revPostingNum = (revPostingNum ff) (revPostingNum fa)
    , lineNum = (lineNum ff) (lineNum fa)
    , date = (date ff) (date fa)
    , flag = (flag ff) (flag fa)
    , number = (number ff) (number fa)
    , payee = (payee ff) (payee fa)
    , account = (account ff) (account fa)
    , postingDrCr = (postingDrCr ff) (postingDrCr fa)
    , postingCmdty = (postingCmdty ff) (postingCmdty fa)
    , postingQty = (postingQty ff) (postingQty fa)
    , totalDrCr = (totalDrCr ff) (totalDrCr fa)
    , totalCmdty = (totalCmdty ff) (totalCmdty fa)
    , totalQty = (totalQty ff) (totalQty fa) }

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

