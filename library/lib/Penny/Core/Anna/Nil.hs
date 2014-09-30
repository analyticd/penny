module Penny.Core.Anna.Nil where

import qualified Penny.Core.Anna.Nil.Ungrouped as NU
import qualified Penny.Core.Anna.Nil.Grouped as NG
import qualified Penny.Core.Anna.Nil.Grouped as Nil.Grouped
import qualified Penny.Core.Anna.Znu1 as Znu1
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Anna.Radun as Radun
import qualified Penny.Core.Anna.RadZ as RadZ
import qualified Penny.Core.Anna.Zng as Zng
import qualified Penny.Core.Anna.NG1 as NG1
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Natural.Unsigned as Unsigned
import qualified Penny.Core.Anna.ZGroup as ZGroup
import qualified Data.Foldable as F

data T r
  = Ungrouped (NU.T r)
  | Grouped (NG.T r)
  deriving (Eq, Ord, Show)

toGravel :: T r -> Gravel.T a
toGravel (Ungrouped (NU.LeadingZero
  (Znu1.T _ Nothing))) = Gravel.T Nothing Exp.Zero

toGravel (Ungrouped (NU.LeadingZero
  (Znu1.T _ (Just (Radun.T _rdx Nothing))))) = Gravel.T Nothing Exp.Zero

toGravel (Ungrouped (NU.LeadingZero
  (Znu1.T _ (Just (Radun.T _rdx (Just (Zeroes.T nz))))))) =
  Gravel.T Nothing . Exp.fromUnsigned . Unsigned.fromNonZero $ nz

toGravel (Ungrouped (NU.NoLeadingZero
  (RadZ.T _rdx zs))) =
  Gravel.T Nothing . Exp.fromUnsigned . Unsigned.fromNonZero
  . Zeroes.numDigits $ zs

toGravel (Grouped (NG.LeadingZero (Zng.T _
  (NG1.T _rdx zs1 _ zs2 zsgs)))) = Gravel.T Nothing . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . Zeroes.toNonZero
  . F.foldr Zeroes.append (Zeroes.append zs1 zs2)
  . fmap ZGroup.zeroes
  $ zsgs

toGravel (Grouped (NG.NoLeadingZero (NG1.T _rdx zs1 _ zs2 zsgs))) =
  Gravel.T Nothing . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . Zeroes.toNonZero
  . F.foldr Zeroes.append (Zeroes.append zs1 zs2)
  . fmap ZGroup.zeroes
  $ zsgs
