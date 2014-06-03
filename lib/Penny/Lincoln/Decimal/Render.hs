-- | Transforms abstract representations into strings.
--
-- Only abstract represenations can be rendered as strings.
module Penny.Lincoln.Decimal.Render where

import Penny.Lincoln.Decimal.Abstract
import qualified Penny.Lincoln.Decimal.Masuno as M
import qualified Penny.Lincoln.Decimal.Frac as F
import Deka.Native.Abstract hiding (Abstract(..))
import Data.List (intersperse)
import qualified Penny.Lincoln.Decimal.Zero as Z
import Penny.Lincoln.Natural

-- | Things that can be rendered.

class Renderable a where
  render :: a -> String

-- | Things that can be rendered, but they need to be passed a
-- 'RadGroup'.

class RenderableRG a where
  renderRG :: RadGroup -> a -> String

-- | The 'Lane' is not rendered; rather, only
-- the digits with appropriate grouping characters are rendered.

instance Renderable Abstract where
  render (Abstract r rg) = case r of
    RFigure f -> case figNonZero f of
      NZMasuno w -> renderRG rg w
      NZFrac fr -> renderRG rg fr
    RZero z -> renderRG rg z

instance RenderableRG M.Masuno where
  renderRG rg =
    either (renderRG rg) (renderRG rg)
    . M.unMasuno

instance RenderableRG M.Monly where
  renderRG r (M.Monly msg lsgs) =
    concat . intersperse ((:[]) . grouper $ r)
    $ render msg : map render lsgs


instance RenderableRG M.Fracuno where
  renderRG r (M.Fracuno msg lsgs fgs)
    = whole ++ rad ++ frac
    where
      rad = (:[]) . radix $ r
      whole = concat . intersperse ((:[]) . grouper $ r)
        $ render msg : map render lsgs
      frac = concat . intersperse ((:[]) . grouper $ r)
        . map render $ fgs

instance RenderableRG Z.Zero where
  renderRG rg = either render (renderRG rg)
    . Z.unZero

instance Renderable Z.PlainZero where
  render _ = "0"

instance RenderableRG Z.GroupedZero where
  renderRG rg (Z.GroupedZero lz g1 gr) =
    lead ++ rad ++ after
    where
      lead = if lz then "0" else ""
      after =
        concat
        . intersperse grp
        . map render
        $ g1:gr
      rad = (:[]) . radix $ rg
      grp = (:[]) . grouper $ rg

instance Renderable Z.Group where
  render = flip replicate '0' . unPositive . Z.unGroup

instance Renderable F.Zeroes where
  render = flip replicate '0' . unPositive . F.unZeroes

instance Renderable F.MSG where
  render (F.MSG zz msd lsd) =
    lead ++ ((:[]) . novemToChar $ msd) ++ map decemToChar lsd
    where
      lead = flip replicate '0' . unNonNegative $ zz

instance Renderable F.LSG where
  render (F.LSG d1 ds) = map decemToChar (d1:ds)

instance RenderableRG F.Frac where
  renderRG rg (F.Frac lz ld ms ls) =
    lead ++ rad ++ concatMap render ld ++ render ms ++
    concatMap render ls
    where
      lead = if lz then "0" else ""
      rad = (:[]) . radix $ rg

instance Renderable M.MSG where
  render (M.MSG nv ds) = novemToChar nv : map decemToChar ds

instance Renderable M.LSG where
  render (M.LSG d1 ds) = map decemToChar (d1:ds)

instance Renderable M.FG where
  render (M.FG d1 ds) = map decemToChar (d1:ds)

radix :: RadGroup -> Char
radix r = case r of
  PeriodComma -> '.'
  PeriodSpace -> '.'
  PeriodThinSpace -> '.'
  CommaPeriod -> ','
  CommaSpace -> ','
  CommaThinSpace -> ','

grouper :: RadGroup -> Char
grouper r = case r of
  PeriodComma -> ','
  PeriodSpace -> ' '
  PeriodThinSpace -> '\x2009'
  CommaPeriod -> '.'
  CommaSpace -> ' '
  CommaThinSpace -> '\x2009'
