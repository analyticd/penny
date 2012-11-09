-- | Calculates cells that "grow to fit." These cells grow to fit the
-- widest cell in the column. No information is ever truncated from
-- these cells (what use is a truncated dollar amount?)
module Penny.Cabin.Posts.Growers (
  GrowOpts(..),
  growCells, Fields(..), grownWidth,
  eFields, EFields(..), pairWithSpacer) where

import Control.Applicative((<$>), Applicative(pure, (<*>)))
import qualified Data.Foldable as Fdbl
import Data.Map (elems, assocs)
import qualified Data.Map as Map
import qualified Data.Semigroup as Semi
import Data.Semigroup ((<>))
import Data.Text (Text, pack, empty)
import qualified Data.Text as X
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Meta as M
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Row as R
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q


-- | All the options needed to grow the cells.
data GrowOpts = GrowOpts {
  baseColors :: CC.BaseColors
  , drCrColors :: CC.DrCrColors
  , dateFormat :: Box -> X.Text
  , qtyFormat :: Box -> X.Text
  , balanceFormat :: L.Commodity -> L.BottomLine -> X.Text
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
growCells ::
  GrowOpts
  -> [Box]
  -> Fields (Maybe ([R.ColumnSpec], Int))
growCells o infos = toPair <$> wanted <*> growers where
  toPair b gwr
    | b =
      let cs = map (gwr o) infos
          w = Fdbl.foldl' f 0 cs where
            f acc c = max acc (widestLine c)
          cs' = map (sizer (R.Width w)) cs
      in if w > 0 then Just (cs', w) else Nothing
    | otherwise = Nothing
  wanted = growingFields . fields $ o

widestLine :: PreSpec -> Int
widestLine (PreSpec _ _ bs) =
  maximum . map (R.unWidth . C.chunkWidth) $ bs

data PreSpec = PreSpec {
  _justification :: R.Justification
  , _padSpec :: C.TextSpec
  , _bits :: [C.Chunk] }


-- | Given a PreSpec and a width, create a ColumnSpec of the right
-- size.
sizer :: R.Width -> PreSpec -> R.ColumnSpec
sizer w (PreSpec j ts bs) = R.ColumnSpec j w ts bs

-- | Makes a left justified cell that is only one line long. The width
-- is unset.
oneLine :: Text -> CC.BaseColors -> Box -> PreSpec
oneLine t bc b =
  let ts = CC.colors (M.visibleNum . L.boxMeta $ b) bc
      j = R.LeftJustify
      bit = C.chunk ts t
  in PreSpec j ts [bit]

-- | Converts a function that accepts a BaseColors to one that accepts
-- a GrowOpts.
bcToGrowOpts ::
  (CC.BaseColors -> Box -> PreSpec)
  -> GrowOpts -> Box -> PreSpec
bcToGrowOpts f g = f (baseColors g)

-- | Converts a function that accepts a DrCrColors to one that accepts
-- a GrowOpts.
dcToGrowOpts ::
  (CC.DrCrColors -> Box -> PreSpec)
  -> GrowOpts -> Box -> PreSpec
dcToGrowOpts f g = f (drCrColors g)


-- | Gets a Fields with each field filled with the function that fills
-- the cells for that field.
growers :: Fields (GrowOpts -> Box -> PreSpec)
growers = Fields {
  globalTransaction      = bcToGrowOpts getGlobalTransaction
  , revGlobalTransaction = bcToGrowOpts getRevGlobalTransaction
  , globalPosting        = bcToGrowOpts getGlobalPosting
  , revGlobalPosting     = bcToGrowOpts getRevGlobalPosting
  , fileTransaction      = bcToGrowOpts getFileTransaction
  , revFileTransaction   = bcToGrowOpts getRevFileTransaction
  , filePosting          = bcToGrowOpts getFilePosting
  , revFilePosting       = bcToGrowOpts getRevFilePosting
  , filtered             = bcToGrowOpts getFiltered
  , revFiltered          = bcToGrowOpts getRevFiltered
  , sorted               = bcToGrowOpts getSorted
  , revSorted            = bcToGrowOpts getRevSorted
  , visible              = bcToGrowOpts getVisible
  , revVisible           = bcToGrowOpts getRevVisible
  , lineNum              = bcToGrowOpts getLineNum
  , date                 = \o -> getDate (baseColors o) (dateFormat o)
  , flag                 = bcToGrowOpts getFlag
  , number               = bcToGrowOpts getNumber
  , postingDrCr          = dcToGrowOpts getPostingDrCr
  , postingCmdty         = dcToGrowOpts getPostingCmdty
  , postingQty           = let fn o =
                                 let fmt = qtyFormat o
                                     dc = drCrColors o
                                 in getPostingQty fmt dc
                           in fn
  , totalDrCr            = dcToGrowOpts getTotalDrCr
  , totalCmdty           = dcToGrowOpts getTotalCmdty
  , totalQty             = let fn o =
                                 let fmt = balanceFormat o
                                     dc = drCrColors o
                                 in getTotalQty fmt dc
                           in fn }


-- | Make a left justified cell one line long that shows a serial.
serialCellMaybe ::
  (L.PostFam -> Maybe Int)
  -- ^ When applied to a Box, this function returns Just Int if the
  -- box has a serial, or Nothing if not.
  
  -> CC.BaseColors -> Box -> PreSpec
serialCellMaybe f bc b = oneLine t bc b
  where
    t = case f (L.boxPostFam b) of
      Nothing -> X.empty
      Just i -> X.pack . show $ i

serialCell ::
  (M.PostMeta -> Int)
  -> CC.BaseColors -> Box -> PreSpec
serialCell f bc b = oneLine t bc b
  where
    t = pack . show . f . L.boxMeta $ b

getGlobalTransaction :: CC.BaseColors -> Box -> PreSpec
getGlobalTransaction =
  serialCellMaybe (fmap (L.forward . L.unGlobalTransaction)
                   . Q.globalTransaction)

getRevGlobalTransaction :: CC.BaseColors -> Box -> PreSpec
getRevGlobalTransaction =
  serialCellMaybe (fmap (L.backward . L.unGlobalTransaction)
                   . Q.globalTransaction)

getGlobalPosting :: CC.BaseColors -> Box -> PreSpec
getGlobalPosting =
  serialCellMaybe (fmap (L.forward . L.unGlobalPosting)
                   . Q.globalPosting)

getRevGlobalPosting :: CC.BaseColors -> Box -> PreSpec
getRevGlobalPosting =
  serialCellMaybe (fmap (L.backward . L.unGlobalPosting)
                   . Q.globalPosting)

getFileTransaction :: CC.BaseColors -> Box -> PreSpec
getFileTransaction =
  serialCellMaybe (fmap (L.forward . L.unFileTransaction)
                   . Q.fileTransaction)

getRevFileTransaction :: CC.BaseColors -> Box -> PreSpec
getRevFileTransaction =
  serialCellMaybe (fmap (L.backward . L.unFileTransaction)
                   . Q.fileTransaction)

getFilePosting :: CC.BaseColors -> Box -> PreSpec
getFilePosting =
  serialCellMaybe (fmap (L.forward . L.unFilePosting)
                   . Q.filePosting)

getRevFilePosting :: CC.BaseColors -> Box -> PreSpec
getRevFilePosting =
  serialCellMaybe (fmap (L.backward . L.unFilePosting)
                   . Q.filePosting)

getSorted :: CC.BaseColors -> Box -> PreSpec
getSorted =
  serialCell (L.forward . Ly.unSortedNum . M.sortedNum)

getRevSorted :: CC.BaseColors -> Box -> PreSpec
getRevSorted =
  serialCell (L.backward . Ly.unSortedNum . M.sortedNum)

getFiltered :: CC.BaseColors -> Box -> PreSpec
getFiltered =
  serialCell (L.forward . Ly.unFilteredNum . M.filteredNum)

getRevFiltered :: CC.BaseColors -> Box -> PreSpec
getRevFiltered =
  serialCell (L.backward . Ly.unFilteredNum . M.filteredNum)

getVisible :: CC.BaseColors -> Box -> PreSpec
getVisible =
  serialCell (L.forward . M.unVisibleNum . M.visibleNum)

getRevVisible :: CC.BaseColors -> Box -> PreSpec
getRevVisible =
  serialCell (L.backward . M.unVisibleNum . M.visibleNum)


getLineNum :: CC.BaseColors -> Box -> PreSpec
getLineNum bc b = oneLine t bc b where
  lineTxt = pack . show . L.unPostingLine
  t = maybe empty lineTxt (Q.postingLine . L.boxPostFam $ b)

getDate :: CC.BaseColors -> (Box -> X.Text) -> Box -> PreSpec
getDate bc gd b = oneLine (gd b) bc b

getFlag :: CC.BaseColors -> Box -> PreSpec
getFlag bc i = oneLine t bc i where
  t = maybe empty L.text (Q.flag . L.boxPostFam $ i)

getNumber :: CC.BaseColors -> Box -> PreSpec
getNumber bc i = oneLine t bc i where
  t = maybe empty L.text (Q.number . L.boxPostFam $ i)

dcTxt :: L.DrCr -> Text
dcTxt L.Debit = pack "Dr"
dcTxt L.Credit = pack "Cr"

-- | Gives a one-line cell that is colored according to whether the
-- posting is a debit or credit.
coloredPostingCell :: Text -> CC.DrCrColors -> Box -> PreSpec
coloredPostingCell t dccol i = PreSpec j ts [bit] where
  j = R.LeftJustify
  bit = C.chunk ts t
  dc = Q.drCr . L.boxPostFam $ i
  ts = CC.colors (M.visibleNum . L.boxMeta $ i)
       . CC.drCrToBaseColors dc
       $ dccol


getPostingDrCr :: CC.DrCrColors -> Box -> PreSpec
getPostingDrCr dc i = coloredPostingCell t dc i where
  t = dcTxt . Q.drCr . L.boxPostFam $ i

getPostingCmdty :: CC.DrCrColors -> Box -> PreSpec
getPostingCmdty dc i = coloredPostingCell t dc i where
  t = L.unCommodity . Q.commodity . L.boxPostFam $ i

getPostingQty :: (Box -> X.Text) -> CC.DrCrColors -> Box -> PreSpec
getPostingQty qf dc i = coloredPostingCell (qf i) dc i

getTotalDrCr :: CC.DrCrColors -> Box -> PreSpec
getTotalDrCr dccol i =
  let vn = M.visibleNum . L.boxMeta $ i
      ts = CC.colors vn bc
      bc = CC.drCrToBaseColors dc dccol
      dc = Q.drCr . L.boxPostFam $ i
      bal = L.unBalance . M.balance . L.boxMeta $ i
      bits =
        if Map.null bal
        then let spec = CC.noBalanceColors vn dccol
             in [C.chunk spec (pack "--")]
        else let toBit bl =
                   let spec = 
                         CC.colors vn
                         . CC.bottomLineToBaseColors dccol
                         $ bl
                       txt = case bl of
                         L.Zero -> pack "--"
                         L.NonZero (L.Column clmDrCr _) ->
                           dcTxt clmDrCr
                   in C.chunk spec txt
             in fmap toBit . elems $ bal
      j = R.LeftJustify
  in PreSpec j ts bits

getTotalCmdty :: CC.DrCrColors -> Box -> PreSpec
getTotalCmdty dccol i =
  let vn = M.visibleNum . L.boxMeta $ i
      j = R.RightJustify
      ts = CC.colors vn bc
      bc = CC.drCrToBaseColors dc dccol
      dc = Q.drCr . L.boxPostFam $ i
      bal = L.unBalance . M.balance . L.boxMeta $ i
      bits =
        if Map.null bal
        then let spec = CC.noBalanceColors vn dccol
             in [C.chunk spec (pack "--")]
        else let toBit (com, nou) =
                   let spec =
                         CC.colors vn
                         . CC.bottomLineToBaseColors dccol
                         $ nou
                       txt = L.unCommodity com
                   in C.chunk spec txt
             in fmap toBit . assocs $ bal
  in PreSpec j ts bits

getTotalQty ::
  (L.Commodity -> L.BottomLine -> X.Text)
  -> CC.DrCrColors
  -> Box
  -> PreSpec
getTotalQty balFmt dccol i =
  let vn = M.visibleNum . L.boxMeta $ i
      j = R.LeftJustify
      ts = CC.colors vn bc
      bc = CC.drCrToBaseColors dc dccol
      dc = Q.drCr . L.boxPostFam $ i
      bal = L.unBalance . M.balance . L.boxMeta $ i
      bits =
        if Map.null bal
        then 
          let spec = CC.noBalanceColors vn dccol
          in [C.chunk spec (pack "--")]
        else
          fmap toChunk . assocs $ bal
            where
              toChunk (com, nou) =
                let spec = 
                      CC.colors vn
                      . CC.bottomLineToBaseColors dccol
                      $ nou
                    txt = balFmt com nou
                in C.chunk spec txt
  in PreSpec j ts bits

growingFields :: F.Fields Bool -> Fields Bool
growingFields f = Fields {
  globalTransaction      = F.globalTransaction    f
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

