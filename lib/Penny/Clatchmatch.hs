-- | Matchers on clatches.
module Penny.Clatchmatch where

import Control.Lens
import Penny.Transbox
import Penny.Ledger
import Penny.Viewpost
import Penny.SeqUtil
import Penny.Amount
import Penny.Converted
import Penny.Filtered
import Penny.Serial
import Penny.Sorted
import Penny.Balance

-- # Transbox

transaction :: Applicative f => Transbox a -> f TransactionL
transaction (Transbox t _) = pure t

-- # View

view :: Applicative f => Transbox (Viewpost a) -> f (View PostingL)
view t = pure (t ^. transboxee.viewpost)

-- # Converted

converted
  :: Applicative f
  => Transbox (Viewpost (Converted a))
  -> f (Maybe Amount)
converted t = pure (t ^. transboxee . viewpostee . Penny.Converted.converted)

preFiltered
  :: Applicative f
  => Transbox (Viewpost (Converted (Filtered a)))
  -> f Serset
preFiltered t
  = pure (t ^. transboxee . viewpostee . convertee . serset)

sorted
  :: Applicative f
  => Transbox (Viewpost (Converted (Filtered (Sorted a))))
  -> f Serset
sorted t
  = pure (t ^. transboxee . viewpostee . convertee . sersetee . serset)

balance
  :: Applicative f
  => Transbox (Viewpost (Converted (Filtered (Sorted (RunningBalance a)))))
  -> f Balance
balance t
  = pure (t ^. transboxee . viewpostee . convertee . sersetee . sersetee
                          . runningBalance )

postFiltered
  :: Applicative f
  => Transbox (Viewpost (Converted (Filtered (Sorted
                          (RunningBalance (Filtered a))))))
  -> f Serset
postFiltered t
  = pure (t ^. transboxee . viewpostee . convertee . sersetee . sersetee
                          . runningBalancee . serset)
