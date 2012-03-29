-- | Calculates cells that "grow to fit." These cells grow to fit the
-- widest cell in the column. No information is ever truncated from
-- these cells (what use is a truncated dollar amount?)
module Penny.Cabin.Posts.Growers (growCells, Fields(..)) where

import Control.Applicative((<$>), Applicative(pure, (<*>)))
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, empty)
import qualified Data.Text as X
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Posts.Colors as PC
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Info as I
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Row as R
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q

-- | Grows the cells that will be GrowToFit cells in the report. First
-- this function fills in all visible cells with text, but leaves the
-- width undetermined. Then it determines the widest line in each
-- column. Finally it adjusts each cell in the column so that it is
-- that maximum width.
--
-- Returns a list of rows, and a Fields holding the width of each
-- cell. Each of these widths will be at least 1; fields that were in
-- the report but that ended up having no width are changed to
-- Nothing.
growCells ::
  Options.T a
  -> [Info.T]
  -> ([Fields (Maybe R.Cell)], Fields (Maybe Int))
growCells o info = (rowsNoZeroes, widthsNoZeroes) where
  cells = justifyCells widths . map (getCells o) $ info
  fieldsInReport = growingFields o
  widths = measureWidest fieldsInReport cells
  widthsNoZeroes = fmap removeZero widths where
    removeZero maybeI = case maybeI of
      Nothing -> Nothing
      Just 0 -> Nothing
      Just x -> Just x
  fieldsNoZeroes = removeZero <$> widths where
    removeZero width maybeCell = case width of
      Nothing -> Nothing
      Just _ -> maybeCell
  rowsNoZeroes = map (fieldsNoZeroes <*>) cells


-- | Given a width and a cell, resizes the cell.
resizer :: Int -> R.Cell -> R.Cell
resizer i c = c { R.width = C.Width i }

-- | Given measurements of the widest cell in a column, adjusts each
-- cell so that it is that wide.
justifyCells ::
  Fields (Maybe Int)
  -> [Fields (Maybe R.Cell)]
  -> [Fields (Maybe R.Cell)]
justifyCells widths cs = let
  justifier mayWidth mayCell = resizer <$> mayWidth <*> mayCell
  justifyRow = newFields justifier widths
  in map justifyRow cs

-- | Measures all cells and returns a Fields indicating the widest
-- field in each column. Fields that are not in the report are
-- Nothing. Fields that are in the report, but that have no width, are
-- Just 0.
measureWidest ::
  Fields Bool
  -> [Fields (Maybe R.Cell)]
  -> Fields (Maybe Int)
measureWidest fs = foldl' updateWidest z where
  z = initWidest fs


-- | Initializes the starting set of fields for the initializer value
-- that is used for updateWidest. Fields that are present in the
-- report are initialized to Just zero; fields not in the report are
-- initialized to Nothing.
initWidest :: Fields Bool -> Fields (Maybe Int)
initWidest = fmap (\b -> if b then Just 0 else Nothing)
  
-- | Given a Fields indicating the cell widths found so far, update
-- the cell widths with values from a new set of cells, keeping
-- whichever is wider. If a cell is not present in the report at all,
-- its corresponding field should be initialized to Nothing; this
-- function will then skip it.
updateWidest ::
  Fields (Maybe Int)
  -> Fields (Maybe R.Cell)
  -> Fields (Maybe Int)
updateWidest = newFields wider where
  wider maybeI maybeC =
    max
    <$> maybeI
    <*> ((C.unWidth . R.widestLine . R.chunks) <$> maybeC)
  
getCells :: Options.T a -> Info.T -> Fields (Maybe R.Cell)
getCells os i = let
  flds = growingFields os
  ifShown fn a =
    if fn flds then Just a else Nothing in
  Fields {
    postingNum = ifShown postingNum (getPostingNum os i)
    , visibleNum = ifShown visibleNum (getVisibleNum os i)
    , revPostingNum = ifShown revPostingNum (getRevPostingNum os i)
    , lineNum = ifShown lineNum (getLineNum os i)
    , date = ifShown date (getDate os i)
    , flag = ifShown flag (getFlag os i)
    , number = ifShown number (getNumber os i)
    , postingDrCr = ifShown postingDrCr (getPostingDrCr os i)
    , postingCmdty = ifShown postingCmdty (getPostingCmdty os i)
    , postingQty = ifShown postingQty (getPostingQty os i)
    , totalDrCr = ifShown totalDrCr (getTotalDrCr os i)
    , totalCmdty = ifShown totalCmdty (getTotalCmdty os i)
    , totalQty = ifShown totalQty (getTotalQty os i) }

-- | Makes a new Fields based on a function and two old Fields. A
-- common pattern is the need to create a new Fields data based on the
-- contents of two old Fields types. This results in a lot of
-- boilerplate. This function takes two Fields data parameterized on
-- different types and a function which takes both of those types and
-- returns a new type. This function is appled to every record in the
-- Fields type to create a new Fields data.
newFields ::
  (a -> b -> c)
  -> Fields a
  -> Fields b
  -> Fields c
newFields f a b =
  f <$> a <*> b
  
{-
  Fields {
  postingNum = f (postingNum a) (postingNum b)
  , visibleNum = f (visibleNum a) (visibleNum b)
  , revPostingNum = f (revPostingNum a) (revPostingNum b)
  , lineNum = f (lineNum a) (lineNum b)
  , date = f (date a) (date b)
  , flag = f (flag a) (flag b)
  , number = f (number a) (number b)
  , postingDrCr = f (postingDrCr a) (postingDrCr b)
  , postingCmdty = f (postingCmdty a) (postingCmdty b)
  , postingQty = f (postingQty a) (postingQty b)
  , totalDrCr = f (totalDrCr a) (totalDrCr b)
  , totalCmdty = f (totalCmdty a) (totalCmdty b)
  , totalQty = f (totalQty a) (totalQty b) }
-}
-- | Makes a left justified cell that is only one line long. The width
-- is unset.
oneLine :: Text -> Options.T a -> Info.T -> R.Cell
oneLine t os i = let
  bc = Options.baseColors os
  vn = I.visibleNum i
  ts = PC.colors vn bc
  w = C.Width 0
  j = R.LeftJustify
  chunk = Seq.singleton . C.chunk ts $ t
  in R.Cell j w ts chunk

getPostingNum :: Options.T a -> Info.T -> R.Cell
getPostingNum os i = oneLine t os i where
  t = pack . show . I.unPostingNum . I.postingNum $ i

getVisibleNum :: Options.T a -> Info.T -> R.Cell
getVisibleNum os i = oneLine t os i where
  t = pack . show . I.unVisibleNum . I.visibleNum $ i

getRevPostingNum :: Options.T a -> Info.T -> R.Cell
getRevPostingNum os i = oneLine t os i where
  t = pack . show . I.unRevPostingNum . I.revPostingNum $ i

getLineNum :: Options.T a -> Info.T -> R.Cell
getLineNum os i = oneLine t os i where
  lineTxt = pack . show . L.unLine . L.unPostingLine
  t = maybe empty lineTxt (Q.postingLine . I.postingBox $ i)

getDate :: Options.T a -> Info.T -> R.Cell
getDate os i = oneLine t os i where
  t = O.dateFormat os i

getFlag :: Options.T a -> Info.T -> R.Cell
getFlag os i = oneLine t os i where
  t = maybe empty L.text (Q.flag . I.postingBox $ i)

getNumber :: Options.T a -> Info.T -> R.Cell
getNumber os i = oneLine t os i where
  t = maybe empty L.text (Q.number . I.postingBox $ i)

dcTxt :: L.DrCr -> Text
dcTxt L.Debit = pack "Dr"
dcTxt L.Credit = pack "Cr"

getPostingDrCr :: Options.T a -> Info.T -> R.Cell
getPostingDrCr os i = oneLine t os i where
  t = dcTxt . Q.drCr . I.postingBox $ i

getPostingCmdty :: Options.T a -> Info.T -> R.Cell
getPostingCmdty os i = oneLine t os i where
  t = L.text . L.Delimited (X.singleton ':') 
      . L.textList . Q.commodity . I.postingBox $ i

getPostingQty :: Options.T a -> Info.T -> R.Cell
getPostingQty os i = oneLine t os i where
  t = O.qtyFormat os i

getTotalDrCr :: Options.T a -> Info.T -> R.Cell
getTotalDrCr os i = let
  vn = I.visibleNum i
  ts = PC.colors vn bc
  bc = PC.drCrToBaseColors dc (O.drCrColors os)
  dc = Q.drCr . I.postingBox $ i
  cs = fmap toChunk
       . Seq.fromList
       . M.elems
       . L.unBalance
       . I.balance
       $ i
  toChunk bl = let
    spec = 
      PC.colors vn
      . PC.bottomLineToBaseColors (O.drCrColors os)
      $ bl
    txt = case bl of
      L.Zero -> pack "--"
      L.NonZero (L.Column clmDrCr _) -> dcTxt clmDrCr
    in C.chunk spec txt
  j = R.LeftJustify
  w = C.Width 0
  in R.Cell j w ts cs

getTotalCmdty :: Options.T a -> Info.T -> R.Cell
getTotalCmdty os i = let
  vn = I.visibleNum i
  j = R.RightJustify
  w = C.Width 0
  ts = PC.colors vn bc
  bc = PC.drCrToBaseColors dc (O.drCrColors os)
  dc = Q.drCr . I.postingBox $ i
  cs = fmap toChunk
       . Seq.fromList
       . M.assocs
       . L.unBalance
       . I.balance
       $ i
  toChunk (com, nou) = let
    spec =
      PC.colors vn
      . PC.bottomLineToBaseColors (O.drCrColors os)
      $ nou
    txt = L.text
          . L.Delimited (X.singleton ':')
          . L.textList
          $ com
    in C.chunk spec txt
  in R.Cell j w ts cs

getTotalQty :: Options.T a -> Info.T -> R.Cell
getTotalQty os i = let
  vn = I.visibleNum i
  j = R.LeftJustify
  ts = PC.colors vn bc
  bc = PC.drCrToBaseColors dc (O.drCrColors os)
  dc = Q.drCr . I.postingBox $ i
  cs = fmap toChunk
       . Seq.fromList
       . M.assocs
       . L.unBalance
       . I.balance
       $ i
  toChunk (com, nou) = let
    spec = 
      PC.colors vn
      . PC.bottomLineToBaseColors (O.drCrColors os)
      $ nou
    txt = O.balanceFormat os com nou
    in C.chunk spec txt
  w = C.Width 0
  in R.Cell j w ts cs

growingFields :: Options.T a -> Fields Bool
growingFields o = let
  f = O.fields o in Fields {
    postingNum = F.postingNum f
    , visibleNum = F.visibleNum f
    , revPostingNum = F.revPostingNum f
    , lineNum = F.lineNum f
    , date = F.date f
    , flag = F.flag f
    , number = F.number f
    , postingDrCr = F.postingDrCr f
    , postingCmdty = F.postingCmdty f
    , postingQty = F.postingQty f
    , totalDrCr = F.totalDrCr f
    , totalCmdty = F.totalCmdty f
    , totalQty = F.totalQty f }

-- | All growing fields.
data Fields a = Fields {
  postingNum :: !a
  , visibleNum :: !a
  , revPostingNum :: !a
  , lineNum :: !a
    -- ^ The line number from the posting's metadata
  , date :: !a
  , flag :: !a
  , number :: !a
  , postingDrCr :: !a
  , postingCmdty :: !a
  , postingQty :: !a
  , totalDrCr :: !a
  , totalCmdty :: !a
  , totalQty :: !a }
  deriving (Show, Eq)

instance Functor Fields where
  fmap f i = Fields {
    postingNum = f (postingNum i)
    , visibleNum = f (visibleNum i)
    , revPostingNum = f (revPostingNum i)
    , lineNum = f (lineNum i)
    , date = f (date i)
    , flag = f (flag i)
    , number = f (number i)
    , postingDrCr = f (postingDrCr i)
    , postingCmdty = f (postingCmdty i)
    , postingQty = f (postingQty i)
    , totalDrCr = f (totalDrCr i)
    , totalCmdty = f (totalCmdty i)
    , totalQty = f (totalQty i) }

instance Applicative Fields where
  pure a = Fields {
    postingNum = a
    , visibleNum = a
    , revPostingNum = a
    , lineNum = a
    , date = a
    , flag = a
    , number = a
    , postingDrCr = a
    , postingCmdty = a
    , postingQty = a
    , totalDrCr = a
    , totalCmdty = a
    , totalQty = a }

  fl <*> fa = Fields {
    postingNum = postingNum fl (postingNum fa)
    , visibleNum = visibleNum fl (visibleNum fa)
    , revPostingNum = revPostingNum fl (revPostingNum fa)
    , lineNum = lineNum fl (lineNum fa)
    , date = date fl (date fa)
    , flag = flag fl (flag fa)
    , number = number fl (number fa)
    , postingDrCr = postingDrCr fl (postingDrCr fa)
    , postingCmdty = postingCmdty fl (postingCmdty fa)
    , postingQty = postingQty fl (postingQty fa)
    , totalDrCr = totalDrCr fl (totalDrCr fa)
    , totalCmdty = totalCmdty fl (totalCmdty fa)
    , totalQty = totalQty fl (totalQty fa) }
    
  


    

{-
t_postingNum :: a -> Fields a -> Fields a
t_postingNum a f = f { postingNum = a }

t_visibleNum :: a -> Fields a -> Fields a
t_visibleNum a f = f { visibleNum = a }

t_revPostingNum :: a -> Fields a -> Fields a
t_revPostingNum a f = f { revPostingNum = a }

t_lineNum :: a -> Fields a -> Fields a
t_lineNum a f = f { lineNum = a }

t_date :: a -> Fields a -> Fields a
t_date a f = f { date = a }

t_flag :: a -> Fields a -> Fields a
t_flag a f = f { flag = a }

t_number :: a -> Fields a -> Fields a
t_number a f = f { number = a }

t_postingDrCr :: a -> Fields a -> Fields a
t_postingDrCr a f = f { postingDrCr = a }

t_postingCmdty :: a -> Fields a -> Fields a
t_postingCmdty a f = f { postingCmdty = a }

t_postingQty :: a -> Fields a -> Fields a
t_postingQty a f = f { postingQty = a }

t_totalDrCr :: a -> Fields a -> Fields a
t_totalDrCr a f = f { totalDrCr = a }

t_totalCmdty :: a -> Fields a -> Fields a
t_totalCmdty a f = f { totalCmdty = a }

t_totalQty :: a -> Fields a -> Fields a
t_totalQty a f = f { totalQty = a }

-}
