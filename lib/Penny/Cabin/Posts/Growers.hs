-- | Calculates cells that "grow to fit." These cells grow to fit the
-- widest cell in the column. No information is ever truncated from
-- these cells (what use is a truncated dollar amount?)
module Penny.Cabin.Posts.Growers (
  growCells, Fields(..), grownWidth,
  eFields, EFields(..), pairWithSpacer) where

import Control.Applicative((<$>), Applicative(pure, (<*>)))
import qualified Data.Foldable as Fdbl
import Data.Map (elems, assocs)
import qualified Data.Semigroup as Semi
import Data.Semigroup ((<>))
import Data.Text (Text, pack, empty)
import qualified Data.Text as X
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Spacers as Spacers
import qualified Penny.Cabin.Row as R
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q


type Box = L.Box M.PostMeta 

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
  Options.T
  -> [Box]
  -> Fields (Maybe ([R.ColumnSpec], Int))
growCells o infos = toPair <$> wanted <*> growers where
  toPair b gwr
    | b = let
      cs = map (gwr o) infos
      w = Fdbl.foldl' f 0 cs where
        f acc c = max acc (widestLine c)
      cs' = map (sizer (R.Width w)) cs
      in if w > 0 then Just (cs', w) else Nothing
    | otherwise = Nothing
  wanted = growingFields o

widestLine :: PreSpec -> Int
widestLine (PreSpec _ _ bs) =
  maximum . map (R.unWidth . C.bitWidth) $ bs

data PreSpec = PreSpec {
  _justification :: R.Justification
  , _padSpec :: C.TextSpec
  , _bits :: [C.Bit] }


-- | Given a PreSpec and a width, create a ColumnSpec of the right
-- size.
sizer :: R.Width -> PreSpec -> R.ColumnSpec
sizer w (PreSpec j ts bs) = R.ColumnSpec j w ts bs

-- | Makes a left justified cell that is only one line long. The width
-- is unset.
oneLine :: Text -> Options.T -> Box -> PreSpec
oneLine t os b =
  let bc = Options.baseColors os
      ts = PC.colors (M.visibleNum . L.boxMeta $ b) bc
      j = R.LeftJustify
      bit = C.bit ts t
  in PreSpec j ts [bit]

growers :: Fields (Options.T -> Box -> PreSpec)
growers = Fields {
  globalTransaction      = getGlobalTransaction
  , revGlobalTransaction = getRevGlobalTransaction
  , globalPosting        = getGlobalPosting
  , revGlobalPosting     = getRevGlobalPosting
  , fileTransaction      = getFileTransaction
  , revFileTransaction   = getRevFileTransaction
  , filePosting          = getFilePosting
  , revFilePosting       = getRevFilePosting
  , filtered             = getFiltered
  , revFiltered          = getRevFiltered
  , sorted               = getSorted
  , revSorted            = getRevSorted
  , visible              = getVisible
  , revVisible           = getRevVisible
  , lineNum              = getLineNum
  , date                 = getDate
  , flag                 = getFlag
  , number               = getNumber
  , postingDrCr          = getPostingDrCr
  , postingCmdty         = getPostingCmdty
  , postingQty           = getPostingQty
  , totalDrCr            = getTotalDrCr
  , totalCmdty           = getTotalCmdty
  , totalQty             = getTotalQty }

-- | Make a left justified cell one line long that shows a serial.
serialCellMaybe ::
  (L.PostFam -> Maybe Int)
  -- ^ When applied to a Box, this function returns Just Int if the
  -- box has a serial, or Nothing if not.
  
  -> Options.T -> Box -> PreSpec
serialCellMaybe f os b = oneLine t os b
  where
    t = case f (L.boxPostFam b) of
      Nothing -> X.empty
      Just i -> X.pack . show $ i

serialCell ::
  (M.PostMeta -> Int)
  -> Options.T -> Box -> PreSpec
serialCell f os b = oneLine t os b
  where
    t = pack . show . f . L.boxMeta $ b

getGlobalTransaction :: Options.T -> Box -> PreSpec
getGlobalTransaction =
  serialCellMaybe (fmap (L.forward . L.unGlobalTransaction)
                   . Q.globalTransaction)

getRevGlobalTransaction :: Options.T -> Box -> PreSpec
getRevGlobalTransaction =
  serialCellMaybe (fmap (L.backward . L.unGlobalTransaction)
                   . Q.globalTransaction)

getGlobalPosting :: Options.T -> Box -> PreSpec
getGlobalPosting =
  serialCellMaybe (fmap (L.forward . L.unGlobalPosting)
                   . Q.globalPosting)

getRevGlobalPosting :: Options.T -> Box -> PreSpec
getRevGlobalPosting =
  serialCellMaybe (fmap (L.backward . L.unGlobalPosting)
                   . Q.globalPosting)

getFileTransaction :: Options.T -> Box -> PreSpec
getFileTransaction =
  serialCellMaybe (fmap (L.forward . L.unFileTransaction)
                   . Q.fileTransaction)

getRevFileTransaction :: Options.T -> Box -> PreSpec
getRevFileTransaction =
  serialCellMaybe (fmap (L.backward . L.unFileTransaction)
                   . Q.fileTransaction)

getFilePosting :: Options.T -> Box -> PreSpec
getFilePosting =
  serialCellMaybe (fmap (L.forward . L.unFilePosting)
                   . Q.filePosting)

getRevFilePosting :: Options.T -> Box -> PreSpec
getRevFilePosting =
  serialCellMaybe (fmap (L.backward . L.unFilePosting)
                   . Q.filePosting)

getSorted :: Options.T -> Box -> PreSpec
getSorted =
  serialCell (L.forward . Ly.unSortedNum . M.sortedNum)

getRevSorted :: Options.T -> Box -> PreSpec
getRevSorted =
  serialCell (L.backward . Ly.unSortedNum . M.sortedNum)

getFiltered :: Options.T -> Box -> PreSpec
getFiltered =
  serialCell (L.forward . Ly.unFilteredNum . M.filteredNum)

getRevFiltered :: Options.T -> Box -> PreSpec
getRevFiltered =
  serialCell (L.backward . Ly.unFilteredNum . M.filteredNum)

getVisible :: Options.T -> Box -> PreSpec
getVisible =
  serialCell (L.forward . M.unVisibleNum . M.visibleNum)

getRevVisible :: Options.T -> Box -> PreSpec
getRevVisible =
  serialCell (L.backward . M.unVisibleNum . M.visibleNum)


getLineNum :: Options.T -> Box -> PreSpec
getLineNum os b = oneLine t os b where
  lineTxt = pack . show . L.unPostingLine
  t = maybe empty lineTxt (Q.postingLine . L.boxPostFam $ b)

getDate :: Options.T -> Box -> PreSpec
getDate os i = oneLine t os i where
  t = O.dateFormat os i

getFlag :: Options.T -> Box -> PreSpec
getFlag os i = oneLine t os i where
  t = maybe empty L.text (Q.flag . L.boxPostFam $ i)

getNumber :: Options.T -> Box -> PreSpec
getNumber os i = oneLine t os i where
  t = maybe empty L.text (Q.number . L.boxPostFam $ i)

dcTxt :: L.DrCr -> Text
dcTxt L.Debit = pack "Dr"
dcTxt L.Credit = pack "Cr"

-- | Gives a one-line cell that is colored according to whether the
-- posting is a debit or credit.
coloredPostingCell :: Text -> Options.T -> Box -> PreSpec
coloredPostingCell t os i = PreSpec j ts [bit] where
  j = R.LeftJustify
  bit = C.bit ts t
  dc = Q.drCr . L.boxPostFam $ i
  ts = PC.colors (M.visibleNum . L.boxMeta $ i)
       . PC.drCrToBaseColors dc
       . O.drCrColors
       $ os

getPostingDrCr :: Options.T -> Box -> PreSpec
getPostingDrCr os i = coloredPostingCell t os i where
  t = dcTxt . Q.drCr . L.boxPostFam $ i

getPostingCmdty :: Options.T -> Box -> PreSpec
getPostingCmdty os i = coloredPostingCell t os i where
  t = L.text . L.Delimited (X.singleton ':') 
      . L.textList . Q.commodity . L.boxPostFam $ i

getPostingQty :: Options.T -> Box -> PreSpec
getPostingQty os i = coloredPostingCell t os i where
  t = O.qtyFormat os i

getTotalDrCr :: Options.T -> Box -> PreSpec
getTotalDrCr os i = let
  vn = M.visibleNum . L.boxMeta $ i
  ts = PC.colors vn bc
  bc = PC.drCrToBaseColors dc (O.drCrColors os)
  dc = Q.drCr . L.boxPostFam $ i
  bits = case M.balance . L.boxMeta $ i of
    Nothing -> let
      spec = PC.noBalanceColors vn (O.drCrColors os)
      in [C.bit spec (pack "--")]
    Just bal -> let
      toBit bl = let
        spec = 
          PC.colors vn
          . PC.bottomLineToBaseColors (O.drCrColors os)
          $ bl
        txt = case bl of
          L.Zero -> pack "--"
          L.NonZero (L.Column clmDrCr _) -> dcTxt clmDrCr
        in C.bit spec txt
      in fmap toBit . elems . L.unBalance $ bal
  j = R.LeftJustify
  in PreSpec j ts bits

getTotalCmdty :: Options.T -> Box -> PreSpec
getTotalCmdty os i = let
  vn = M.visibleNum . L.boxMeta $ i
  j = R.RightJustify
  ts = PC.colors vn bc
  bc = PC.drCrToBaseColors dc (O.drCrColors os)
  dc = Q.drCr . L.boxPostFam $ i
  bits = case M.balance . L.boxMeta $ i of
    Nothing -> let
      spec = PC.noBalanceColors vn (O.drCrColors os)
      in [C.bit spec (pack "--")]
    Just bal -> let
      toBit (com, nou) = let
        spec =
          PC.colors vn
          . PC.bottomLineToBaseColors (O.drCrColors os)
          $ nou
        txt = L.text
              . L.Delimited (X.singleton ':')
              . L.textList
              $ com
        in C.bit spec txt
      in fmap toBit . assocs . L.unBalance $ bal
  in PreSpec j ts bits

getTotalQty :: Options.T -> Box -> PreSpec
getTotalQty os i = let
  vn = M.visibleNum . L.boxMeta $ i
  j = R.LeftJustify
  ts = PC.colors vn bc
  bc = PC.drCrToBaseColors dc (O.drCrColors os)
  dc = Q.drCr . L.boxPostFam $ i
  bits = case M.balance . L.boxMeta $ i of
    Nothing -> let
      spec = PC.noBalanceColors vn (O.drCrColors os)
      in [C.bit spec (pack "--")]
    Just bal -> fmap toChunk . assocs . L.unBalance $ bal where
      toChunk (com, nou) = let
        spec = 
          PC.colors vn
          . PC.bottomLineToBaseColors (O.drCrColors os)
          $ nou
        txt = O.balanceFormat os com nou
        in C.bit spec txt
  in PreSpec j ts bits

growingFields :: Options.T -> Fields Bool
growingFields o = let
  f = O.fields o in Fields {
    globalTransaction      = F.globalTransaction  f
    , revGlobalTransaction = F.globalTransaction  f
    , globalPosting        = F.globalPosting      f
    , revGlobalPosting     = F.revGlobalPosting   f
    , fileTransaction      = F.fileTransaction    f
    , revFileTransaction   = F.revFileTransaction f
    , filePosting          = F.filePosting        f
    , revFilePosting       = F.revFilePosting     f
    , filtered             = F.filtered           f
    , revFiltered          = F.revFiltered        f
    , sorted               = F.sorted             f
    , revSorted            = F.revSorted          f
    , visible              = F.visible            f
    , revVisible           = F.revVisible         f
    , lineNum              = F.lineNum            f
    , date                 = F.date               f
    , flag                 = F.flag               f
    , number               = F.number             f
    , postingDrCr          = F.postingDrCr        f
    , postingCmdty         = F.postingCmdty       f
    , postingQty           = F.postingQty         f
    , totalDrCr            = F.totalDrCr          f
    , totalCmdty           = F.totalCmdty         f
    , totalQty             = F.totalQty           f }

-- | All growing fields, as an ADT.
data EFields =
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
  | EPostingDrCr
  | EPostingCmdty
  | EPostingQty
  | ETotalDrCr
  | ETotalCmdty
  | ETotalQty
  deriving (Show, Eq, Ord, Enum)

-- | Returns a Fields where each record has its corresponding EField.
eFields :: Fields EFields
eFields = Fields {
  globalTransaction      = EGlobalTransaction
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
  , postingDrCr          = EPostingDrCr
  , postingCmdty         = EPostingCmdty
  , postingQty           = EPostingQty
  , totalDrCr            = ETotalDrCr
  , totalCmdty           = ETotalCmdty
  , totalQty             = ETotalQty }

-- | All growing fields.
data Fields a = Fields {
  globalTransaction      :: a
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
  , postingDrCr          :: a
  , postingCmdty         :: a
  , postingQty           :: a
  , totalDrCr            :: a
  , totalCmdty           :: a
  , totalQty             :: a }
  deriving (Show, Eq)

instance Fdbl.Foldable Fields where
  foldr f z i =
    f (globalTransaction i)
    (f (revGlobalTransaction i)
     (f (globalPosting i)
      (f (revGlobalPosting i)
       (f (fileTransaction i)
        (f (revFileTransaction i)
         (f (filePosting i)
          (f (revFilePosting i)
           (f (filtered i)
            (f (revFiltered i)
             (f (sorted i)
              (f (revSorted i)
               (f (visible i)
                (f (revVisible i)
                 (f (lineNum i)
                  (f (date i)
                   (f (flag i)
                    (f (number i)
                     (f (postingDrCr i)
                      (f (postingCmdty i)
                       (f (postingQty i)
                        (f (totalDrCr i)
                         (f (totalCmdty i)
                          (f (totalQty i) z)))))))))))))))))))))))

instance Functor Fields where
  fmap f i = Fields {
    globalTransaction      = f (globalTransaction    i)
    , revGlobalTransaction = f (revGlobalTransaction i)
    , globalPosting        = f (globalPosting        i)
    , revGlobalPosting     = f (revGlobalPosting     i)
    , fileTransaction      = f (fileTransaction      i)
    , revFileTransaction   = f (revFileTransaction   i)
    , filePosting          = f (filePosting          i)
    , revFilePosting       = f (revFilePosting       i)
    , filtered             = f (filtered             i)
    , revFiltered          = f (revFiltered          i)
    , sorted               = f (sorted               i)
    , revSorted            = f (revSorted            i)
    , visible              = f (visible              i)
    , revVisible           = f (revVisible           i)
    , lineNum              = f (lineNum              i)
    , date                 = f (date                 i)
    , flag                 = f (flag                 i)
    , number               = f (number               i)
    , postingDrCr          = f (postingDrCr          i)
    , postingCmdty         = f (postingCmdty         i)
    , postingQty           = f (postingQty           i)
    , totalDrCr            = f (totalDrCr            i)
    , totalCmdty           = f (totalCmdty           i)
    , totalQty             = f (totalQty             i) }

instance Applicative Fields where
  pure a = Fields {
    globalTransaction      = a
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
    , postingDrCr          = a
    , postingCmdty         = a
    , postingQty           = a
    , totalDrCr            = a
    , totalCmdty           = a
    , totalQty             = a }

  fl <*> fa = Fields {
    globalTransaction      = globalTransaction    fl (globalTransaction    fa)
    , revGlobalTransaction = revGlobalTransaction fl (revGlobalTransaction fa)
    , globalPosting        = globalPosting        fl (globalPosting        fa)
    , revGlobalPosting     = revGlobalPosting     fl (revGlobalPosting     fa)
    , fileTransaction      = fileTransaction      fl (fileTransaction      fa)
    , revFileTransaction   = revFileTransaction   fl (revFileTransaction   fa)
    , filePosting          = filePosting          fl (filePosting          fa)
    , revFilePosting       = revFilePosting       fl (revFilePosting       fa)
    , filtered             = filtered             fl (filtered             fa)
    , revFiltered          = revFiltered          fl (revFiltered          fa)
    , sorted               = sorted               fl (sorted               fa)
    , revSorted            = revSorted            fl (revSorted            fa)
    , visible              = visible              fl (visible              fa)
    , revVisible           = revVisible           fl (revVisible           fa)
    , lineNum              = lineNum              fl (lineNum              fa)
    , date                 = date                 fl (date                 fa)
    , flag                 = flag                 fl (flag                 fa)
    , number               = number               fl (number               fa)
    , postingDrCr          = postingDrCr          fl (postingDrCr          fa)
    , postingCmdty         = postingCmdty         fl (postingCmdty         fa)
    , postingQty           = postingQty           fl (postingQty           fa)
    , totalDrCr            = totalDrCr            fl (totalDrCr            fa)
    , totalCmdty           = totalCmdty           fl (totalCmdty           fa)
    , totalQty             = totalQty             fl (totalQty             fa) }

-- | Pairs data from a Fields with its matching spacer field. The
-- spacer field is returned in a Maybe because the TotalQty field does
-- not have a spacer.
pairWithSpacer :: Fields a -> Spacers.T b -> Fields (a, Maybe b)
pairWithSpacer f s = Fields {
  globalTransaction      = (globalTransaction    f, Just (S.globalTransaction    s))
  , revGlobalTransaction = (revGlobalTransaction f, Just (S.revGlobalTransaction s))
  , globalPosting        = (globalPosting        f, Just (S.globalPosting        s))
  , revGlobalPosting     = (revGlobalPosting     f, Just (S.revGlobalPosting     s))
  , fileTransaction      = (fileTransaction      f, Just (S.fileTransaction      s))
  , revFileTransaction   = (revFileTransaction   f, Just (S.revFileTransaction   s))
  , filePosting          = (filePosting          f, Just (S.filePosting          s))
  , revFilePosting       = (revFilePosting       f, Just (S.revFilePosting       s))
  , filtered             = (filtered             f, Just (S.filtered             s))
  , revFiltered          = (revFiltered          f, Just (S.revFiltered          s))
  , sorted               = (sorted               f, Just (S.sorted               s))
  , revSorted            = (revSorted            f, Just (S.revSorted            s))
  , visible              = (visible              f, Just (S.visible              s))
  , revVisible           = (revVisible           f, Just (S.revVisible           s))
  , lineNum              = (lineNum              f, Just (S.lineNum              s))
  , date                 = (date                 f, Just (S.date                 s))
  , flag                 = (flag                 f, Just (S.flag                 s))
  , number               = (number               f, Just (S.number               s))
  , postingDrCr          = (postingDrCr          f, Just (S.postingDrCr          s))
  , postingCmdty         = (postingCmdty         f, Just (S.postingCmdty         s))
  , postingQty           = (postingQty           f, Just (S.postingQty           s))
  , totalDrCr            = (totalDrCr            f, Just (S.totalDrCr            s))
  , totalCmdty           = (totalCmdty           f, Just (S.totalCmdty           s))
  , totalQty             = (totalQty             f, Nothing                        ) }

-- | Reduces a set of Fields to a single value.
reduce :: Semi.Semigroup s => Fields s -> s
reduce f =
  globalTransaction       f
  <> revGlobalTransaction f
  <> globalPosting        f
  <> revGlobalPosting     f
  <> fileTransaction      f
  <> revFileTransaction   f
  <> filePosting          f
  <> revFilePosting       f
  <> filtered             f
  <> revFiltered          f
  <> sorted               f
  <> revSorted            f
  <> visible              f
  <> revVisible           f
  <> lineNum              f
  <> date                 f
  <> flag                 f
  <> number               f
  <> postingDrCr          f
  <> postingCmdty         f
  <> postingQty           f
  <> totalDrCr            f
  <> totalCmdty           f
  <> totalQty             f

-- | Compute the width of all Grown cells, including any applicable
-- spacer cells.
grownWidth ::
  Fields (Maybe Int)
  -> Spacers.T Int
  -> Int
grownWidth fs ss =
  Semi.getSum
  . reduce
  . fmap Semi.Sum
  . fmap fieldWidth
  $ pairWithSpacer fs ss

-- | Compute the field width of a single field and its spacer. The
-- first element of the tuple is the field width, if present; the
-- second element of the tuple is the width of the spacer. If there is
-- no field, returns 0.
fieldWidth :: (Maybe Int, Maybe Int) -> Int
fieldWidth (m1, m2) = case m1 of
  Nothing -> 0
  Just i1 -> case m2 of
    Just i2 -> if i2 > 0 then i1 + i2 else i1
    Nothing -> i1

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
