module Penny.Tree.Lewis where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as S
import qualified Penny.Tree.Masuno1 as Masuno1
import qualified Penny.Tree.Masuno1Radix1 as Masuno1Radix1
import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Tree.LZ1 as LZ1
import qualified Penny.Tree.LR1 as LR1
import Text.Parsec.Text
import qualified Penny.Tree.Parsec as P
import Control.Applicative
import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna as Anna
import qualified Penny.Core.Anna.BrimUngrouped as BrimUngrouped
import qualified Penny.Core.Anna.BrimGrouped as BrimGrouped
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.Nodbu as Nodbu
import qualified Penny.Core.Anna.Decems as Decems
import qualified Penny.Core.Anna.Radem as Radem
import qualified Penny.Core.Anna.DecDecs as DecDecs

-- | The root of parse trees for number representations.
data T a
  = Novem NovDecs.T (Maybe (Masuno1.T a))
  | Zero Zero.T (Maybe (LZ1.T a))
  | Radix (Radix.T a) (LR1.T a)
  deriving (Eq, Ord, Show)

parser :: Parser (Radix.T a) -> Parser a -> Parser (T a)
parser pr pa =
  Novem <$> NovDecs.parser
        <*> optional (Masuno1.parser pr pa)

  <|> Zero <$> Zero.parser <*> optional (LZ1.parser pr pa)
  <|> Radix <$> pr <*> LR1.parser pa

toAnna :: T a -> Anna.T a
toAnna t = case t of
  Novem nd1 mayMas -> Anna.Brim $ case mayMas of
    Nothing -> Brim.Ungrouped . BrimUngrouped.Masuno
      $ Nodbu.T nd1 Nothing
    Just (Masuno1.T rdx maym1r1) -> case maym1r1 of
      Nothing -> Brim.Ungrouped . BrimUngrouped.Masuno
        $ Nodbu.T nd1 (Just (Radem.T rdx Decems.empty))
      Just (Masuno1Radix1.T dds gs) -> case S.viewl gs of
        EmptyL -> Brim.Ungrouped . BrimUngrouped.Masuno
          $ Nodbu.T nd1 (Just (Radem.T rdx (DecDecs.toDecems dds)))
{-
        grp1 :< grpRest -> Brim.Grouped $ BrimGrouped.Masuno nd1
          (BG1.GroupOnRight rdx (DecDecsMayGroups.T dds
-}
