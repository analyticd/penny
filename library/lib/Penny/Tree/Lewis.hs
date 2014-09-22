module Penny.Tree.Lewis where

import Data.Sequence (ViewL(..), (|>), (<|))
import qualified Data.Sequence as S
import qualified Penny.Tree.Masuno1 as Masuno1
import qualified Penny.Tree.Masuno1Radix1 as Masuno1Radix1
import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Tree.LZ1 as LZ1
import qualified Penny.Tree.LZ2 as LZ2
import qualified Penny.Tree.LZ3 as LZ3
import qualified Penny.Tree.LZ4 as LZ4
import qualified Penny.Tree.LZ6 as LZ6
import qualified Penny.Tree.LR1 as LR1
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.Radun as Radun
import qualified Penny.Core.Anna as Anna
import qualified Penny.Core.Anna.BrimUngrouped as BrimUngrouped
import qualified Penny.Core.Anna.BrimGrouped as BrimGrouped
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.Nodbu as Nodbu
import qualified Penny.Core.Anna.Decems as Decems
import qualified Penny.Core.Anna.Radem as Radem
import qualified Penny.Core.Anna.DecDecs as DecDecs
import qualified Penny.Core.Anna.BG1 as BG1
import qualified Penny.Core.Anna.BG4 as BG4
import qualified Penny.Core.Anna.BG5 as BG5
import qualified Penny.Core.Anna.BG6 as BG6
import qualified Penny.Core.Anna.BG7 as BG7
import qualified Penny.Core.Anna.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.Core.Anna.Nodecs3 as Nodecs3
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.Nil.Ungrouped as NilU
import qualified Penny.Core.Anna.Nil.Grouped as NilG
import qualified Penny.Core.Anna.Znu1 as Znu1
import qualified Penny.Core.Anna.Zng as Zng
import qualified Penny.Core.Anna.NG1 as NG1
import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.BU2 as BU2
import qualified Penny.Core.Anna.BU3 as BU3
import qualified Penny.Core.Anna.Zerabu as Zerabu
import qualified Penny.Core.Anna.Zenod as Zenod
import qualified Penny.Core.Anna.ZGroup as ZGroup

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
        grp1 :< grpRest -> Brim.Grouped $ BrimGrouped.Masuno nd1
          (BG1.GroupOnRight rdx dds grp1 grpRest)

  Zero z1 mayZs -> case mayZs of
    Nothing -> Anna.Nil . Nil.Ungrouped . NilU.LeadingZero
      $ Znu1.T z1 Nothing
    Just (LZ1.T rdx mayLz2) -> case mayLz2 of
      Nothing -> Anna.Nil . Nil.Ungrouped . NilU.LeadingZero
        $ Znu1.T z1 (Just (Radun.T rdx Nothing))
      Just lz2 -> case lz2 of
        LZ2.Zero zs mayLz3 -> case mayLz3 of
          Nothing -> Anna.Nil . Nil.Ungrouped . NilU.LeadingZero
            $ Znu1.T z1 (Just (Radun.T rdx (Just zs)))
          Just lz3 -> case lz3 of
            LZ3.Novem nd mayLz4 -> case mayLz4 of
              Nothing -> Anna.Brim . Brim.Ungrouped
                . BrimUngrouped.Fracuno . BU2.LeadingZero
                $ Zerabu.T z1 rdx (BU3.Zeroes (Zenod.T zs nd))
              Just (LZ4.T g1 gs) -> Anna.Brim . Brim.Grouped
                . BrimGrouped.Fracuno
                $ BG4.T (Just z1) rdx (BG5.Zeroes zs
                  (BG6.Novem (NovSeqDecsNE.T nd
                    (SeqDecsNE.T g1 (SeqDecs.T gs)))))
            LZ3.Group g lz6 -> case lz6 of
              LZ6.Novem nd sd -> Anna.Brim . Brim.Grouped
                . BrimGrouped.Fracuno
                $ BG4.T (Just z1) rdx (BG5.Zeroes zs
                  (BG6.Group g (BG7.LeadNovem (Nodecs3.T nd sd))))
              LZ6.Zero zs2 mayEi -> case mayEi of
                Nothing -> Anna.Nil . Nil.Grouped
                  . NilG.LeadingZero . Zng.T z1
                  $ NG1.T rdx zs g zs2 S.empty
                Just ei -> case ei of
                  Left (nd, sd) -> Anna.Brim . Brim.Grouped
                    . BrimGrouped.Fracuno
                    . BG4.T (Just z1) rdx
                    . BG5.Zeroes zs
                    . BG6.Group g
                    . BG7.LeadZeroes zs2
                    . Right
                    . Nodecs3.T nd
                    $ sd
                  Right (g2, lz62) -> undefined

procLZ3fromLZ6
  :: LZ6.T g
  -> Either (BG7.T g) (Zeroes.T, S.Seq (ZGroup.T g))
procLZ3fromLZ6 lz6 = case lz6 of
  LZ6.Novem nd sd -> Left (BG7.LeadNovem (Nodecs3.T nd sd))
  LZ6.Zero zs my -> case my of
    Nothing -> Right (zs, S.empty)
    Just ei -> case ei of
      Left (nd, sd) -> Left (BG7.LeadZeroes zs
        (Right (Nodecs3.T nd sd)))
      Right (g, lz6') -> procLZ6Recurse S.empty (zs, g) lz6'

procLZ6Recurse
  :: S.Seq (Zeroes.T, g)
  -> (Zeroes.T, g)
  -> LZ6.T g
  -> Either (BG7.T g) (Zeroes.T, S.Seq (ZGroup.T g))
procLZ6Recurse sq (zs, g) lz6 = case S.viewl sq of
  EmptyL -> case lz6 of
    LZ6.Novem nd sq' -> Left (BG7.LeadZeroes zs (Left
      (g, BG7.LeadNovem (Nodecs3.T nd sq'))))
    LZ6.Zero zs' my -> case my of
      Nothing -> Right (zs, S.singleton (ZGroup.T g zs'))
      Just ei -> case ei of
        Left (nd, sd) -> Left (BG7.LeadZeroes zs (Left (g,
          BG7.LeadZeroes zs' (Right (Nodecs3.T nd sd)))))
        Right (g', lz6') -> procLZ6Recurse (sq |> (zs, g)) (zs', g') lz6'
  (zsl, gl) :< rest -> undefined

{-
case lz6 of
  LZ6.Novem nd sd -> case S.viewl sq of
    EmptyL -> Left $ BG7.LeadZeroes zs (Left (g,
      BG7.Novem (Nodecs3.T nd sd)))
    (zsl, gl) :< rest -> 
-}
{-
      Right (g', lz6') -> Left (BG7.LeadZeroes 
      Right (g', lz6') -> procLZ3fromLZ6Recurse
        S.empty (g, zs) g' lz6'
-}


{-
procLZ3fromLZ6Recurse
  :: S.Seq (g, Zeroes.T)
  -- ^ Groups so far
  -> (g, Zeroes.T)
  -- ^ Rightmost group so far
  -> g
  -- ^ Rightmost grouping character
  -> LZ6.T g
  -> Either (BG7.T g) (g, Zeroes.T, S.Seq (ZGroup.T g))
procLZ3fromLZ6Recurse sq (rg, rz) g lz6 = case lz6 of
  LZ6.Novem nd sd -> Left $ mkBG7LeadZeroes sq (rg, rz) g nd sd
  LZ6.Zero zs my -> case my of
    Nothing -> Right $ mkZeroTrip sq (rg, rz) g zs
    Just ei -> case ei of
      Left (nd, sd) -> Left $ mkBG7MoreZeroes sq (rg, rz) g zs nd sd
      Right (g', lz6') -> procLZ3fromLZ6Recurse
        (sq |> (rg, rz)) (g, zs) g' lz6'

mkBG7LeadZeroes
  :: S.Seq (g, Zeroes.T)
  -> (g, Zeroes.T)
  -> g
  -> NovDecs.T
  -> SeqDecs.T g
  -> BG7.T g
mkBG7LeadZeroes sq (g1, zs) g2 nd sd = case S.viewl sq of
  EmptyL -> undefined

mkZeroTrip
  :: S.Seq (g, Zeroes.T)
  -> (g, Zeroes.T)
  -> g
  -> Zeroes.T
  -> (g, Zeroes.T, S.Seq (ZGroup.T g))
mkZeroTrip sq (g1, zs1) g2 zs2 = case S.viewl sq of
  EmptyL -> (g1, zs1, S.singleton (ZGroup.T g2 zs2))
  (gf, zf) :< pr -> (gf, zf,
    (ZGroup.T g1 zs1) <| (ZGroup.T g2 zs2) <| fmap (uncurry ZGroup.T) pr)

mkBG7MoreZeroes
  :: S.Seq (g, Zeroes.T)
  -> (g, Zeroes.T)
  -> g
  -> Zeroes.T
  -> NovDecs.T
  -> SeqDecs.T g
  -> BG7.T g
mkBG7MoreZeroes = undefined

{-
procLZ6fromLZ3 :: LZ6.T a -> Either (BG7.T a) Zeroes.T
procLZ6fromLZ3 lz6 = case lz6 of
  LZ6.Novem nd sd -> Left $ BG7.LeadNovem (Nodecs3.T nd sd)
  LZ6.Zero zs my -> case my of
    Nothing -> Right zs
    Just ei -> case ei of
      Left (nd, sd) -> Left $ BG7.LeadZeroes zs
        (Right (Nodecs3.T nd sd))
      Right (g, lz6') -> Left $ procLZ6fromLZ3Recurse zs g lz6'

procLZ6fromLZ3Recurse
  :: Zeroes.T
  -> a
  -> LZ6.T a
  -> BG7.T a
procLZ6fromLZ3Recurse zs g lz6 = case lz6 of
  LZ6.Novem nd sd -> BG7.LeadZeroes zs
    (Left (g, BG7.LeadNovem $ Nodecs3.T nd SeqDecs.empty))
  LZ6.Zero zs' my -> case my of
    Nothing -> BG7.LeadZeroes zs
      (Left (g, BG7.LeadZeroes zs 
-}
-}
