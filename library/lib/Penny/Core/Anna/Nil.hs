module Penny.Core.Anna.Nil where

import qualified Penny.Core.Anna.Nil.Ungrouped as NU
import qualified Penny.Core.Anna.Nil.Grouped as NG
import qualified Penny.Core.Anna.Znu1 as Znu1
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Anna.Radun as Radun
import qualified Penny.Core.Anna.RadZ as RadZ
import qualified Penny.Core.Anna.Zng as Zng
import qualified Penny.Core.Anna.NG1 as NG1
import qualified Penny.Natural.Unsigned as Unsigned
import qualified Penny.Core.Anna.ZGroup as ZGroup
import qualified Data.Foldable as F

data T r
  = Ungrouped (NU.T r)
  | Grouped (NG.T r)
  deriving (Eq, Ord, Show)

toExp :: T r -> Exp.T
toExp (Ungrouped (NU.LeadingZero
  (Znu1.T _ Nothing))) = Exp.Zero

toExp (Ungrouped (NU.LeadingZero
  (Znu1.T _ (Just (Radun.T _rdx Nothing))))) = Exp.Zero

toExp (Ungrouped (NU.LeadingZero
  (Znu1.T _ (Just (Radun.T _rdx (Just (Zeroes.T nz))))))) =
  Exp.fromUnsigned . Unsigned.fromNonZero $ nz

toExp (Ungrouped (NU.NoLeadingZero
  (RadZ.T _rdx zs))) =
  Exp.fromUnsigned . Unsigned.fromNonZero . Zeroes.numDigits $ zs

toExp (Grouped (NG.LeadingZero (Zng.T _
  (NG1.T _rdx zs1 _ zs2 zsgs)))) = Exp.fromUnsigned
  . Unsigned.fromNonZero
  . Zeroes.toNonZero
  . F.foldr Zeroes.append (Zeroes.append zs1 zs2)
  . fmap ZGroup.zeroes
  $ zsgs

toExp (Grouped (NG.NoLeadingZero (NG1.T _rdx zs1 _ zs2 zsgs))) =
    Exp.fromUnsigned
  . Unsigned.fromNonZero
  . Zeroes.toNonZero
  . F.foldr Zeroes.append (Zeroes.append zs1 zs2)
  . fmap ZGroup.zeroes
  $ zsgs
