-- | Fields that can appear in the Posts report.
module Penny.Cabin.Posts.Fields where

import Control.Applicative(Applicative(pure, (<*>)))
import qualified Data.Foldable as F

data T a = T {
  globalTransaction :: a
  , revGlobalTransaction :: a
  , globalPosting :: a
  , revGlobalPosting :: a
  , fileTransaction :: a
  , revFileTransaction :: a
  , filePosting :: a
  , revFilePosting :: a
  , filtered :: a
  , revFiltered :: a
  , sorted :: a
  , revSorted :: a
  , visible :: a
  , revVisible :: a
  , lineNum :: a
    -- ^ The line number from the posting's metadata
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
  , totalQty :: a
  , tags :: a
  , memo :: a
  , filename :: a }
  deriving (Show, Eq)

instance Functor T where
  fmap f fa = T {
    globalTransaction = f (globalTransaction fa)
    , revGlobalTransaction = f (revGlobalTransaction fa)
    , globalPosting = f (globalPosting fa)
    , revGlobalPosting = f (revGlobalPosting fa)
    , fileTransaction = f (fileTransaction fa)
    , revFileTransaction = f (revFileTransaction fa)
    , filePosting = f (filePosting fa)
    , revFilePosting = f (revFilePosting fa)
    , filtered = f (filtered fa)
    , revFiltered = f (revFiltered fa)
    , sorted = f (sorted fa)
    , revSorted = f (revSorted fa)
    , visible = f (visible fa)
    , revVisible = f (revVisible fa)
    , lineNum = f (lineNum fa)
    , date = f (date fa)
    , flag = f (flag fa)
    , number = f (number fa)
    , payee = f (payee fa)
    , account = f (account fa)
    , postingDrCr = f (postingDrCr fa)
    , postingCmdty = f (postingCmdty fa)
    , postingQty = f (postingQty fa)
    , totalDrCr = f (totalDrCr fa)
    , totalCmdty = f (totalCmdty fa)
    , totalQty = f (totalQty fa)
    , tags = f (tags fa)
    , memo = f (memo fa)
    , filename = f (filename fa) }

instance Applicative T where
  pure a = T {
    globalTransaction = a
    , revGlobalTransaction = a
    , globalPosting = a
    , revGlobalPosting = a
    , fileTransaction = a
    , revFileTransaction = a
    , filePosting = a
    , revFilePosting = a
    , filtered = a
    , revFiltered = a
    , sorted = a
    , revSorted = a
    , visible = a
    , revVisible = a
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
    , totalQty = a
    , tags = a
    , memo = a
    , filename = a }

  ff <*> fa = T {
    globalTransaction = globalTransaction ff (globalTransaction fa)
    , revGlobalTransaction = revGlobalTransaction ff
                             (revGlobalTransaction fa)
    , globalPosting = globalPosting ff (globalPosting fa)
    , revGlobalPosting = revGlobalPosting ff (revGlobalPosting fa)
    , fileTransaction = fileTransaction ff (fileTransaction fa)
    , revFileTransaction = revFileTransaction ff (revFileTransaction fa)
    , filePosting = filePosting ff (filePosting fa)
    , revFilePosting = revFilePosting ff (revFilePosting fa)
    , filtered = filtered ff (filtered fa)
    , revFiltered = revFiltered ff (revFiltered fa)
    , sorted = sorted ff (sorted fa)
    , revSorted = revSorted ff (revSorted fa)
    , visible = visible ff (visible fa)
    , revVisible = revVisible ff (revVisible fa)
    , lineNum = lineNum ff (lineNum fa)
    , date = date ff (date fa)
    , flag = flag ff (flag fa)
    , number = number ff (number fa)
    , payee = payee ff (payee fa)
    , account = account ff (account fa)
    , postingDrCr = postingDrCr ff (postingDrCr fa)
    , postingCmdty = postingCmdty ff (postingCmdty fa)
    , postingQty = postingQty ff (postingQty fa)
    , totalDrCr = totalDrCr ff (totalDrCr fa)
    , totalCmdty = totalCmdty ff (totalCmdty fa)
    , totalQty = totalQty ff (totalQty fa)
    , tags = tags ff (tags fa)
    , memo = memo ff (memo fa)
    , filename = filename ff (filename fa) }

instance F.Foldable T where
  foldr f z t =
    f (globalTransaction t)
    (f (revGlobalTransaction t)
     (f (globalPosting t)
      (f (revGlobalPosting t)
       (f (fileTransaction t)
        (f (revFileTransaction t)
         (f (filePosting t)
          (f (revFilePosting t)
           (f (filtered t)
            (f (revFiltered t)
             (f (sorted t)
              (f (revSorted t)
               (f (visible t)
                (f (revVisible t)
                 (f (lineNum t)
                  (f (date t)
                   (f (flag t)
                    (f (number t)
                     (f (payee t)
                      (f (account t)
                       (f (postingDrCr t)
                        (f (postingCmdty t)
                         (f (postingQty t)
                          (f (totalDrCr t)
                           (f (totalCmdty t)
                            (f (totalQty t)
                             (f (tags t)
                              (f (memo t)
                               (f (filename t) z))))))))))))))))))))))))))))


t_globalTransaction :: a -> T a -> T a
t_globalTransaction a f = f { globalTransaction = a }

t_revGlobalTransaction :: a -> T a -> T a
t_revGlobalTransaction a f = f { revGlobalTransaction = a }

t_globalPosting :: a -> T a -> T a
t_globalPosting a f = f { globalPosting = a }

t_revGlobalPosting :: a -> T a -> T a
t_revGlobalPosting a f = f { revGlobalPosting = a }

t_fileTransaction :: a -> T a -> T a
t_fileTransaction a f = f { fileTransaction = a }

t_revFileTransaction :: a -> T a -> T a
t_revFileTransaction a f = f { revFileTransaction = a }

t_filePosting :: a -> T a -> T a
t_filePosting a f = f { filePosting = a }

t_revFilePosting :: a -> T a -> T a
t_revFilePosting a f = f { revFilePosting = a }

t_filtered :: a -> T a -> T a
t_filtered a f = f { filtered = a }

t_revFiltered :: a -> T a -> T a
t_revFiltered a f = f { revFiltered = a }

t_sorted :: a -> T a -> T a
t_sorted a f = f { sorted = a }

t_revSorted :: a -> T a -> T a
t_revSorted a f = f { revSorted = a }

t_visible :: a -> T a -> T a
t_visible a f = f { visible = a }

t_revVisible :: a -> T a -> T a
t_revVisible a f = f { revVisible = a }

t_lineNum :: a -> T a -> T a
t_lineNum a f = f { lineNum = a }

t_date :: a -> T a -> T a
t_date a f = f { date = a }

t_flag :: a -> T a -> T a
t_flag a f = f { flag = a }

t_number :: a -> T a -> T a
t_number a f = f { number = a }

t_payee :: a -> T a -> T a
t_payee a f = f { payee = a }

t_account :: a -> T a -> T a
t_account a f = f { account = a }

t_postingDrCr :: a -> T a -> T a
t_postingDrCr a f = f { postingDrCr = a }

t_postingCmdty :: a -> T a -> T a
t_postingCmdty a f = f { postingCmdty = a }

t_postingQty :: a -> T a -> T a
t_postingQty a f = f { postingQty = a }

t_totalDrCr :: a -> T a -> T a
t_totalDrCr a f = f { totalDrCr = a }

t_totalCmdty :: a -> T a -> T a
t_totalCmdty a f = f { totalCmdty = a }

t_totalQty :: a -> T a -> T a
t_totalQty a f = f { totalQty = a }

t_tags :: a -> T a -> T a
t_tags a f = f { tags = a }

t_memo :: a -> T a -> T a
t_memo a f = f { memo = a }

t_filename :: a -> T a -> T a
t_filename a f = f { filename = a }

