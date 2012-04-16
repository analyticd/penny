-- | Options for the Balance report.
module Penny.Cabin.Balance.Options where

import qualified Data.Text as X
import qualified Penny.Lincoln as L
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Options as O
import qualified Penny.Shield as S
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Lincoln.Balance as Bal

data Options = Options {
  drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.BottomLine -> X.Text
  , colorPref :: Chunk.Colors
  , removeZeroBalances :: RemoveZeroBalances
  }

newtype RemoveZeroBalances =
  RemoveZeroBalances { unRemoveZeroBalances :: Bool }
  deriving (Show, Eq)

balanceAsIs :: L.BottomLine -> X.Text
balanceAsIs n = case n of
  L.Zero -> X.pack "--"
  L.NonZero c -> X.pack . show . L.unQty . Bal.qty $ c

defaultOptions :: S.Runtime -> Options
defaultOptions rt = Options {
  drCrColors = DB.drCrColors
  , baseColors = DB.baseColors
  , balanceFormat = balanceAsIs
  , colorPref = O.maxCapableColors rt
  , removeZeroBalances = RemoveZeroBalances False }
