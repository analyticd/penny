{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,
             MultiParamTypeClasses #-}
module Penny.Copper.LincolnTypes where

import Control.Applicative
import Data.Sequence (Seq)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Penny.Lincoln.Rep
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin
import Penny.Copper.Parser

pNovem :: Parser Novem
pNovem =
  (D1 <$ pSym '1')
  <|> (D2 <$ pSym '2')
  <|> (D3 <$ pSym '3')
  <|> (D4 <$ pSym '4')
  <|> (D5 <$ pSym '5')
  <|> (D6 <$ pSym '6')
  <|> (D7 <$ pSym '7')
  <|> (D8 <$ pSym '8')
  <|> (D9 <$ pSym '9')

rNovem :: Novem -> ShowS
rNovem n = case n of
  { D1 -> ('1':); D2 -> ('2':); D3 -> ('3':); D4 -> ('4':); D5 -> ('5':);
    D6 -> ('6':); D7 -> ('7':); D8 -> ('8':); D9 -> ('9':) }

pDecem :: Parser Decem
pDecem = (D0 <$ pSym '0') <|> fmap Nonem pNovem

rDecem :: Decem -> ShowS
rDecem d = case d of { D0 -> ('0':); Nonem n -> rNovem n }

pGrouper :: Parser Grouper
pGrouper = ThinSpace <$ pSym '\x2009'
    <|> Underscore <$ pSym '_'

rGrouper :: Grouper -> ShowS
rGrouper g = case g of { ThinSpace -> ('\x2009':); Underscore -> ('_':) }

pRadCom :: Parser RadCom
pRadCom = Period <$ pSym '.'
    <|> RCGrouper <$> pGrouper

rRadCom :: RadCom -> ShowS
rRadCom r = case r of { Period -> ('.':); RCGrouper g -> rGrouper g }

pRadixRadCom :: Parser (Radix RadCom)
pRadixRadCom = Radix <$ pSym ','

rRadixRadCom :: Radix RadCom -> ShowS
rRadixRadCom Radix = (',':)

pRadPer :: Parser RadPer
pRadPer = Comma <$ pSym ','
    <|> RPGrouper <$> pGrouper

rRadPer :: RadPer -> ShowS
rRadPer c = case c of { Comma -> (',':); RPGrouper g -> rGrouper g }

pRadixRadPer :: Parser (Radix RadPer)
pRadixRadPer = Radix <$ pSym '.'

rRadixRadPer :: Radix RadPer -> ShowS
rRadixRadPer Radix = ('.':)

pSide :: Parser Side
pSide = Debit <$ pSym '<' <|> Credit <$ pSym '>'

rSide :: Side -> ShowS
rSide s = case s of { Debit -> ('<':); Credit -> ('>':) }

pZero :: Parser Zero
pZero = Zero <$ pSym '0'

rZero :: Zero -> ShowS
rZero Zero = ('0':)

pSeqDecs :: Parser g -> Parser (Seq (g, Decem, Seq Decem))
pSeqDecs pg = pSeq ((,,) <$> pg <*> pDecem <*> pSeq pDecem)

rSeqDecs :: (g -> ShowS) -> (Seq (g, Decem, Seq Decem)) -> ShowS
rSeqDecs f = rSeq (\(g, d1, ds) -> f g . rDecem d1 . rSeq rDecem ds)

pNilGrouped :: Parser (Radix r) -> Parser r -> Parser (NilGrouped r)
pNilGrouped pr pg =
  NilGrouped
  <$> optional pZero
  <*> pr
  <*> pZero
  <*> pSeq pZero
  <*> pg
  <*> pZero
  <*> pSeq pZero
  <*> pSeq ((,,) <$> pg <*> pZero <*> pSeq pZero)

rMaybe :: (r -> ShowS) -> Maybe r -> ShowS
rMaybe rr = maybe id rr

rNilGrouped :: (Radix r -> ShowS) -> (r -> ShowS) -> NilGrouped r -> ShowS
rNilGrouped rr rg (NilGrouped z0 r1 z2 sz3 g4 z5 sz6 sg7)
  = rMaybe rZero z0
  . rr r1
  . rZero z2
  . rSeq rZero sz3
  . rg g4
  . rZero z5
  . rSeq rZero sz6
  . rSeq (\(r8, z9, sz10) -> rg r8 . rZero z9 . rSeq rZero sz10) sg7

pNilUngrouped :: Parser (Radix r) -> Parser (NilUngrouped r)
pNilUngrouped pr = pNUZero <|> pNURadix
  where
    pNUZero = NUZero
      <$> pZero
      <*> optional
            ((,) <$> pr
                 <*> optional ((,) <$> pZero <*> pSeq pZero))
    pNURadix = NURadix
      <$> pr
      <*> pZero
      <*> pSeq pZero

rNilUngrouped :: (Radix r -> ShowS) -> NilUngrouped r -> ShowS
rNilUngrouped rr nu = case nu of
  NUZero z0 m1 ->
      rZero z0
    . case m1 of
        Nothing -> id
        Just (r2, m3) -> rr r2 . case m3 of
          Nothing -> id
          Just (z4, sz5) -> rZero z4 . rSeq rZero sz5

  NURadix r1 z2 sz3 -> rr r1 . rZero z2 . rSeq rZero sz3

pBG7 :: Parser r -> Parser (BG7 r)
pBG7 pg = pz <|> pn
  where
    pz = BG7Zeroes
      <$> pZero
      <*> pSeq pZero
      <*> pBG8 pg
    pn = BG7Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pSeqDecs pg

rBG7 :: (r -> ShowS) -> BG7 r -> ShowS
rBG7 rr bg7 = case bg7 of
  BG7Zeroes z0 sz1 bg8 -> rZero z0 . rSeq rZero sz1 . rBG8 rr bg8
  BG7Novem n0 sd1 sq2 -> rNovem n0 . rSeq rDecem sd1 . rSeqDecs rr sq2

pBG8 :: Parser r -> Parser (BG8 r)
pBG8 pg = pnv <|> pgrp
  where
    pnv = BG8Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pSeqDecs pg
    pgrp = BG8Group
      <$> pg
      <*> pBG7 pg

rBG8 :: (r -> ShowS) -> BG8 r -> ShowS
rBG8 rr bg8 = case bg8 of
  BG8Novem n0 sd1 sd2 -> rNovem n0 . rSeq rDecem sd1 . rSeqDecs rr sd2
  BG8Group g bg7 -> rr g . rBG7 rr bg7

pBG6 :: Parser r -> Parser (BG6 r)
pBG6 pg = pnv <|> pgrp
  where
    pnv = BG6Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pg
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg
    pgrp = BG6Group <$> pg <*> pBG7 pg

rBG6 :: (r -> ShowS) -> BG6 r -> ShowS
rBG6 rr x = case x of
  BG6Novem n0 sd1 g2 d3 sd4 sd5 ->
    rNovem n0
    . rSeq rDecem sd1
    . rr g2
    . rDecem d3
    . rSeq rDecem sd4
    . rSeqDecs rr sd5
  BG6Group r0 bg7 -> rr r0 . rBG7 rr bg7

pBG5 :: Parser r -> Parser (BG5 r)
pBG5 pg = pnv <|> pz
  where
    pnv = BG5Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pg
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg
    pz = BG5Zero
      <$> pZero
      <*> pSeq pZero
      <*> pBG6 pg

rBG5 :: (r -> ShowS) -> BG5 r -> ShowS
rBG5 rg x = case x of
  BG5Novem n0 sd1 g2 d3 sd4 sdd5 ->
    rNovem n0
    . rSeq rDecem sd1
    . rg g2
    . rDecem d3
    . rSeq rDecem sd4
    . rSeqDecs rg sdd5
  BG5Zero z0 sz0 bg6 -> rZero z0 . rSeq rZero sz0 . rBG6 rg bg6

pBG1 :: Parser (Radix r) -> Parser r -> Parser (BG1 r)
pBG1 pr pg = onLeft <|> onRight
  where
    onLeft = BG1GroupOnLeft
      <$> pg
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg
      <*> optional
          ( (,) <$> pr <*> optional
              (((,,) <$> pDecem <*> pSeq pDecem <*> pSeqDecs pg)))

    onRight = BG1GroupOnRight
      <$> pr
      <*> pDecem
      <*> pSeq pDecem
      <*> pg <*> pDecem <*> pSeq pDecem
      <*> pSeqDecs pg

rBG1 :: (Radix r -> ShowS) -> (r -> ShowS) -> BG1 r -> ShowS
rBG1 rr rg x = case x of
  BG1GroupOnLeft r0 d1 sd2 sd3 m4
    -> rg r0
    . rDecem d1
    . rSeq rDecem sd2
    . rSeqDecs rg sd3
    . case m4 of
        Nothing -> id
        Just (r5, m6) -> rr r5 . case m6 of
          Nothing -> id
          Just (d7, sd8, sd9) -> rDecem d7 . rSeq rDecem sd8
            . rSeqDecs rg sd9
  BG1GroupOnRight r0 d1 sd2 g3 d4 ds5 sd6 ->
    rr r0 . rDecem d1 . rSeq rDecem sd2
    . rg g3 . rDecem d4 . rSeq rDecem ds5 . rSeqDecs rg sd6

pBrimUngrouped :: Parser (Radix r) -> Parser (BrimUngrouped r)
pBrimUngrouped pr = gtOne <|> ltOne
  where
    gtOne = BUGreaterThanOne
      <$> pNovem
      <*> pSeq pDecem
      <*> optional ((,) <$> pr <*> pSeq pDecem)
    ltOne = BULessThanOne
      <$> optional pZero
      <*> pr
      <*> pSeq pZero
      <*> pNovem
      <*> pSeq pDecem

rBrimUngrouped :: (Radix r -> ShowS) -> BrimUngrouped r -> ShowS
rBrimUngrouped rr x = case x of
  BUGreaterThanOne n0 sd1 m2 -> rNovem n0 . rSeq rDecem sd1
    . case m2 of
        Nothing -> id
        Just (r3, sd4) -> rr r3 . rSeq rDecem sd4
  BULessThanOne mz0 r1 sz2 n3 sd4 -> rMaybe rZero mz0
    . rr r1 . rSeq rZero sz2 . rNovem n3 . rSeq rDecem sd4

pBrimGrouped :: Parser (Radix r) -> Parser r -> Parser (BrimGrouped r)
pBrimGrouped pr pg = gtOne <|> ltOne
  where
    gtOne = BGGreaterThanOne
      <$> pNovem
      <*> pSeq pDecem
      <*> pBG1 pr pg
    ltOne = BGLessThanOne
      <$> optional pZero
      <*> pr
      <*> pBG5 pg

rBrimGrouped :: (Radix r -> ShowS) -> (r -> ShowS) -> BrimGrouped r -> ShowS
rBrimGrouped rr rg x = case x of
  BGGreaterThanOne n0 sd1 bg1 -> rNovem n0 . rSeq rDecem sd1 . rBG1 rr rg bg1
  BGLessThanOne mz0 r1 bg5 -> rMaybe rZero mz0
    . rr r1 . rBG5 rg bg5

pBrim :: Parser (Radix r) -> Parser r -> Parser (Brim r)
pBrim pr pg = BrimGrouped <$> pBrimGrouped pr pg
    <|> BrimUngrouped <$> pBrimUngrouped pr

rBrim :: (Radix r -> ShowS) -> (r -> ShowS) -> Brim r -> ShowS
rBrim rr rg x = case x of
  BrimGrouped bg -> rBrimGrouped rr rg bg
  BrimUngrouped bu -> rBrimUngrouped rr bu

pNil :: Parser (Radix r) -> Parser r -> Parser (Nil r)
pNil pr pg = NilU <$> pNilUngrouped pr
    <|> NilG <$> pNilGrouped pr pg

rNil :: (Radix r -> ShowS) -> (r -> ShowS) -> Nil r -> ShowS
rNil rr rg x = case x of
  NilU nu -> rNilUngrouped rr nu
  NilG ng -> rNilGrouped rr rg ng

pNilOrBrimScalar
  :: Parser (Radix r)
  -> Parser r
  -> Parser (NilOrBrimScalar r)
pNilOrBrimScalar pr pg = fmap NilOrBrimScalar $ Left <$> pNil pr pg
    <|> Right <$> pBrim pr pg

rNilOrBrimScalar
  :: (Radix r -> ShowS)
  -> (r -> ShowS)
  -> NilOrBrimScalar r
  -> ShowS
rNilOrBrimScalar rr rg (NilOrBrimScalar ei) = case ei of
  Left n -> rNil rr rg n
  Right b -> rBrim rr rg b

pPluMin :: Parser PluMin
pPluMin = Plus <$ pSym '+' <|> Minus <$ pSym '-'

rPluMin :: PluMin -> ShowS
rPluMin Plus = ('+':)
rPluMin Minus = ('-':)


