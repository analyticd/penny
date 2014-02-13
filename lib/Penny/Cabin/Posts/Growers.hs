{-# LANGUAGE OverloadedStrings #-}
-- | Calculates cells that "grow to fit." These cells grow to fit the
-- widest cell in the column. No information is ever truncated from
-- these cells (what use is a truncated dollar amount?)
module Penny.Cabin.Posts.Growers (
  GrowOpts(..),
  growCells, Fields(..), grownWidth,
  eFields, EFields(..), pairWithSpacer) where

import Control.Applicative((<$>), Applicative(pure, (<*>)))
import qualified Data.Foldable as Fdbl
import Data.Map (elems)
import qualified Data.Map as Map
import qualified Data.Semigroup as Semi
import Data.Semigroup ((<>), mempty)
import Data.Text (Text, pack, empty)
import qualified Data.Text as X
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified System.Console.Rainbow as Rb


-- | All the options needed to grow the cells.
data GrowOpts = GrowOpts
  { dateFormat :: (M.PostMeta, L.Posting) -> X.Text
  , qtyFormat :: L.Amount L.Qty -> X.Text
  , fields :: F.Fields Bool
  }

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
growCells
  :: E.Changers
  -> GrowOpts
  -> [(M.PostMeta, L.Posting)]
  -> Fields (Maybe ([R.ColumnSpec], Int))
growCells ch o infos = toPair <$> wanted <*> growers where
  toPair b gwr
    | b =
      let cs = map (gwr o ch) infos
          w = Fdbl.foldl' f 0 cs where
            f acc c = max acc (widestLine c)
          cs' = map (sizer (R.Width w)) cs
      in if w > 0 then Just (cs', w) else Nothing
    | otherwise = Nothing
  wanted = growingFields . fields $ o

widestLine :: PreSpec -> Int
widestLine (PreSpec _ _ bs) =
  case bs of
    [] -> 0
    xs -> maximum . map (X.length . Rb.text) $ xs

data PreSpec = PreSpec {
  _justification :: R.Justification
  , _padSpec :: (E.Label, E.EvenOdd)
  , _bits :: [Rb.Chunk] }


-- | Given a PreSpec and a width, create a ColumnSpec of the right
-- size.
sizer :: R.Width -> PreSpec -> R.ColumnSpec
sizer w (PreSpec j ts bs) = R.ColumnSpec j w ts bs

-- | Makes a left justified cell that is only one line long. The width
-- is unset.
oneLine :: E.Changers -> Text -> E.Label -> (M.PostMeta, L.Posting) -> PreSpec
oneLine chgrs t lbl b =
  let eo = E.fromVisibleNum . M.visibleNum . fst $ b
      j = R.LeftJustify
      md = E.getEvenOddLabelValue lbl eo chgrs
      ck = [md $ Rb.Chunk mempty t]
  in PreSpec j (lbl, eo) ck


-- | Gets a Fields with each field filled with the function that fills
-- the cells for that field.
growers :: Fields (GrowOpts -> E.Changers -> (M.PostMeta, L.Posting) -> PreSpec)
growers = Fields
  { globalTransaction    = const getGlobalTransaction
  , revGlobalTransaction = const getRevGlobalTransaction
  , globalPosting        = const getGlobalPosting
  , revGlobalPosting     = const getRevGlobalPosting
  , fileTransaction      = const getFileTransaction
  , revFileTransaction   = const getRevFileTransaction
  , filePosting          = const getFilePosting
  , revFilePosting       = const getRevFilePosting
  , filtered             = const getFiltered
  , revFiltered          = const getRevFiltered
  , sorted               = const getSorted
  , revSorted            = const getRevSorted
  , visible              = const getVisible
  , revVisible           = const getRevVisible
  , lineNum              = const getLineNum
  , date                 = \o ch -> getDate ch (dateFormat o)
  , flag                 = const getFlag
  , number               = const getNumber
  , postingDrCr          = const getPostingDrCr
  , postingCmdty         = const getPostingCmdty
  , postingQty           = \o ch -> getPostingQty ch (qtyFormat o)
  , totalDrCr            = const getTotalDrCr
  , totalCmdty           = const getTotalCmdty
  , totalQty             = \o ch -> getTotalQty ch (qtyFormat o)
  }

-- | Make a left justified cell one line long that shows a serial.
serialCellMaybe
  :: E.Changers
  -> (L.Posting -> Maybe Int)
  -- ^ When applied to a Box, this function returns Just Int if the
  -- box has a serial, or Nothing if not.

  -> (M.PostMeta, L.Posting) -> PreSpec
serialCellMaybe chgrs f b = oneLine chgrs t E.Other b
  where
    t = case f (snd b) of
      Nothing -> X.empty
      Just i -> X.pack . show $ i

serialCell
  :: E.Changers
  -> (M.PostMeta -> Int)
  -> (M.PostMeta, L.Posting) -> PreSpec
serialCell chgrs f b = oneLine chgrs t E.Other b
  where
    t = pack . show . f . fst $ b

getGlobalTransaction :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getGlobalTransaction chgrs =
  serialCellMaybe chgrs (fmap (L.forward . L.unGlobalTransaction)
                        . Q.globalTransaction)

getRevGlobalTransaction :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevGlobalTransaction chgrs =
  serialCellMaybe chgrs (fmap (L.backward . L.unGlobalTransaction)
                        . Q.globalTransaction)

getGlobalPosting :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getGlobalPosting chgrs =
  serialCellMaybe chgrs (fmap (L.forward . L.unGlobalPosting)
                        . Q.globalPosting)

getRevGlobalPosting :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevGlobalPosting chgrs =
  serialCellMaybe chgrs (fmap (L.backward . L.unGlobalPosting)
                   . Q.globalPosting)

getFileTransaction :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getFileTransaction chgrs =
  serialCellMaybe chgrs (fmap (L.forward . L.unFileTransaction)
                   . Q.fileTransaction)

getRevFileTransaction :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevFileTransaction chgrs =
  serialCellMaybe chgrs (fmap (L.backward . L.unFileTransaction)
                   . Q.fileTransaction)

getFilePosting :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getFilePosting chgrs =
  serialCellMaybe chgrs (fmap (L.forward . L.unFilePosting)
                   . Q.filePosting)

getRevFilePosting :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevFilePosting chgrs =
  serialCellMaybe chgrs (fmap (L.backward . L.unFilePosting)
                   . Q.filePosting)

getSorted :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getSorted chgrs =
  serialCell chgrs (L.forward . Ly.unSortedNum . M.sortedNum)

getRevSorted :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevSorted chgrs =
  serialCell chgrs (L.backward . Ly.unSortedNum . M.sortedNum)

getFiltered :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getFiltered chgrs =
  serialCell chgrs (L.forward . Ly.unFilteredNum . M.filteredNum)

getRevFiltered :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevFiltered chgrs =
  serialCell chgrs (L.backward . Ly.unFilteredNum . M.filteredNum)

getVisible :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getVisible chgrs =
  serialCell chgrs (L.forward . M.unVisibleNum . M.visibleNum)

getRevVisible :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getRevVisible chgrs =
  serialCell chgrs (L.backward . M.unVisibleNum . M.visibleNum)


getLineNum :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getLineNum chgrs b = oneLine chgrs t E.Other b where
  lineTxt = pack . show . L.unPostingLine
  t = maybe empty lineTxt (Q.postingLine . snd $ b)

getDate :: E.Changers -> ((M.PostMeta, L.Posting) -> X.Text) -> (M.PostMeta, L.Posting) -> PreSpec
getDate chgrs gd b = oneLine chgrs (gd b) E.Other b

getFlag :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getFlag chgrs i = oneLine chgrs t E.Other i where
  t = maybe empty L.text (Q.flag . snd $ i)

getNumber :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getNumber chgrs i = oneLine chgrs t E.Other i where
  t = maybe empty L.text (Q.number . snd $ i)

dcTxt :: L.DrCr -> Text
dcTxt L.Debit = X.singleton '<'
dcTxt L.Credit = X.singleton '>'

-- | Gives a one-line cell that is colored according to whether the
-- posting is a debit or credit.
coloredPostingCell :: E.Changers -> Text -> (M.PostMeta, L.Posting) -> PreSpec
coloredPostingCell chgrs t i = PreSpec j (lbl, eo) [bit] where
  j = R.LeftJustify
  lbl = case Q.drCr . snd $ i of
    L.Debit -> E.Debit
    L.Credit -> E.Credit
  eo = E.fromVisibleNum . M.visibleNum . fst $ i
  md = E.getEvenOddLabelValue lbl eo chgrs
  bit = md $ Rb.Chunk mempty t


getPostingDrCr :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getPostingDrCr ch i = coloredPostingCell ch t i where
  t = dcTxt . Q.drCr . snd $ i

getPostingCmdty :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getPostingCmdty ch i = coloredPostingCell ch t i where
  t = L.unCommodity . Q.commodity . snd $ i

getPostingQty
  :: E.Changers
  -> (L.Amount L.Qty -> X.Text) -- ((M.PostMeta, L.Posting) -> X.Text)
  -> (M.PostMeta, L.Posting)
  -> PreSpec
getPostingQty ch qf i = coloredPostingCell ch qtyStr i
  where
    qtyStr = case (L.entry . L.headEnt . snd . L.unPosting . snd $ i) of
      Left qr -> L.showQtyRep . L.qty . L.amount $ qr
      Right q -> qf . L.amount $ q

getTotalDrCr :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getTotalDrCr ch i =
  let vn = M.visibleNum . fst $ i
      ps = (lbl, eo)
      dc = Q.drCr . snd $ i
      lbl = E.dcToLbl dc
      eo = E.fromVisibleNum vn
      bal = L.unBalance . M.balance . fst $ i
      md = E.getEvenOddLabelValue lbl eo ch
      bits =
        if Map.null bal
        then [md "--"]
        else let mkChk e = E.bottomLineToDrCr mayDc eo ch
                  where
                    mayDc = case e of
                      L.Zero -> Nothing
                      L.NonZero c -> Just $ L.colDrCr c
             in fmap mkChk . elems $ bal
      j = R.LeftJustify
  in PreSpec j ps bits

getTotalCmdty :: E.Changers -> (M.PostMeta, L.Posting) -> PreSpec
getTotalCmdty ch i =
  let vn = M.visibleNum . fst $ i
      j = R.RightJustify
      ps = (lbl, eo)
      dc = Q.drCr . snd $ i
      eo = E.fromVisibleNum vn
      lbl = E.dcToLbl dc
      bal = Map.toList . L.unBalance . M.balance . fst $ i
      preChunks = E.balancesToCmdtys ch eo bal
  in PreSpec j ps preChunks

getTotalQty
  :: E.Changers
  -> (L.Amount L.Qty -> X.Text)
  -> (M.PostMeta, L.Posting)
  -> PreSpec
getTotalQty ch balFmt i =
  let vn = M.visibleNum . fst $ i
      j = R.LeftJustify
      dc = Q.drCr . snd $ i
      ps = (E.dcToLbl dc, eo)
      eo = E.fromVisibleNum vn
      bal = Map.toList . L.unBalance . M.balance . fst $ i
      preChunks = E.balanceToQtys ch balFmt eo bal
  in PreSpec j ps preChunks

growingFields :: F.Fields Bool -> Fields Bool
growingFields f = Fields
  { globalTransaction    = F.globalTransaction    f
  , revGlobalTransaction = F.revGlobalTransaction f
  , globalPosting        = F.globalPosting        f
  , revGlobalPosting     = F.revGlobalPosting     f
  , fileTransaction      = F.fileTransaction      f
  , revFileTransaction   = F.revFileTransaction   f
  , filePosting          = F.filePosting          f
  , revFilePosting       = F.revFilePosting       f
  , filtered             = F.filtered             f
  , revFiltered          = F.revFiltered          f
  , sorted               = F.sorted               f
  , revSorted            = F.revSorted            f
  , visible              = F.visible              f
  , revVisible           = F.revVisible           f
  , lineNum              = F.lineNum              f
  , date                 = F.date                 f
  , flag                 = F.flag                 f
  , number               = F.number               f
  , postingDrCr          = F.postingDrCr          f
  , postingCmdty         = F.postingCmdty         f
  , postingQty           = F.postingQty           f
  , totalDrCr            = F.totalDrCr            f
  , totalCmdty           = F.totalCmdty           f
  , totalQty             = F.totalQty             f }

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
eFields = Fields
  { globalTransaction     = EGlobalTransaction
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
data Fields a = Fields
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
  fmap f i = Fields
    { globalTransaction    = f (globalTransaction    i)
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
  pure a = Fields
    { globalTransaction     = a
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

  fl <*> fa = Fields
    { globalTransaction    = globalTransaction    fl (globalTransaction    fa)
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
pairWithSpacer :: Fields a -> S.Spacers b -> Fields (a, Maybe b)
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
  -> S.Spacers Int
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

