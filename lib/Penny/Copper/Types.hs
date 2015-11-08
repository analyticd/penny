{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,
             MultiParamTypeClasses #-}
module Penny.Copper.Types where

import Control.Applicative (many, Alternative, (<|>), optional)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Megaparsec (char, try)
import Text.Megaparsec.Text (Parser)

import Penny.Representation
import Penny.Polar
import Penny.PluMin

pSeq :: Parser a -> Parser (Seq a)
pSeq p = fmap Seq.fromList $ many p

rSeq :: (a -> ShowS) -> Seq a -> ShowS
rSeq f = foldr (.) id . fmap f

pEither :: Alternative f => f a -> f b -> f (Either a b)
pEither l r = Left <$> l <|> Right <$> r

rList :: (a -> ShowS) -> [a] -> ShowS
rList f = foldr (.) id . fmap f


pZero :: Parser Zero
pZero = Zero <$ char '0'

rZero :: Zero -> ShowS
rZero Zero = ('0':)

pOne :: Parser One
pOne = One <$ char '1'

rOne :: One -> ShowS
rOne One = ('1':)

pTwo :: Parser Two
pTwo = Two <$ char '2'

rTwo :: Two -> ShowS
rTwo Two = ('2':)

pThree :: Parser Three
pThree = Three <$ char '3'

rThree :: Three -> ShowS
rThree Three = ('3':)

pFour :: Parser Four
pFour = Four <$ char '4'

rFour :: Four -> ShowS
rFour Four = ('4':)

pFive :: Parser Five
pFive = Five <$ char '5'

rFive :: Five -> ShowS
rFive Five = ('5':)

pSix :: Parser Six
pSix = Six <$ char '6'

rSix :: Six -> ShowS
rSix Six = ('6':)

pSeven :: Parser Seven
pSeven = Seven <$ char '7'

rSeven :: Seven -> ShowS
rSeven Seven = ('7':)

pEight :: Parser Eight
pEight = Eight <$ char '8'

rEight :: Eight -> ShowS
rEight Eight = ('8':)

pNine :: Parser Nine
pNine = Nine <$ char '9'

rNine :: Nine -> ShowS
rNine Nine = ('9':)

pD8 :: Parser D8
pD8 =
  (D8'1 <$ char '1')
  <|> (D8'2 <$ char '2')
  <|> (D8'3 <$ char '3')
  <|> (D8'4 <$ char '4')
  <|> (D8'5 <$ char '5')
  <|> (D8'6 <$ char '6')
  <|> (D8'7 <$ char '7')
  <|> (D8'8 <$ char '8')


rD8 :: D8 -> ShowS
rD8 n = case n of
  { D8'1 -> ('1':); D8'2 -> ('2':); D8'3 -> ('3':); D8'4 -> ('4':);
    D8'5 -> ('5':); D8'6 -> ('6':); D8'7 -> ('7':); D8'8 -> ('8':) }

pD8z :: Parser D8z
pD8z =
      (D8z'0 <$ char '0')
  <|> (D8z'1 <$ char '1')
  <|> (D8z'2 <$ char '2')
  <|> (D8z'3 <$ char '3')
  <|> (D8z'4 <$ char '4')
  <|> (D8z'5 <$ char '5')
  <|> (D8z'6 <$ char '6')
  <|> (D8z'7 <$ char '7')
  <|> (D8z'8 <$ char '8')


rD8z :: D8z -> ShowS
rD8z n = case n of
  { D8z'0 -> ('0':); D8z'1 -> ('1':); D8z'2 -> ('2':); D8z'3 -> ('3':);
    D8z'4 -> ('4':); D8z'5 -> ('5':); D8z'6 -> ('6':); D8z'7 -> ('7':);
    D8z'8 -> ('8':) }

pD9 :: Parser D9
pD9 =
  (D9'1 <$ char '1')
  <|> (D9'2 <$ char '2')
  <|> (D9'3 <$ char '3')
  <|> (D9'4 <$ char '4')
  <|> (D9'5 <$ char '5')
  <|> (D9'6 <$ char '6')
  <|> (D9'7 <$ char '7')
  <|> (D9'8 <$ char '8')
  <|> (D9'9 <$ char '9')

rD9 :: D9 -> ShowS
rD9 n = case n of
  { D9'1 -> ('1':); D9'2 -> ('2':); D9'3 -> ('3':); D9'4 -> ('4':);
    D9'5 -> ('5':);
    D9'6 -> ('6':); D9'7 -> ('7':); D9'8 -> ('8':); D9'9 -> ('9':) }

pD9z :: Parser D9z
pD9z = (D9z'0 <$ char '0') <|> fmap c'D9z'D9 pD9

rD9z :: D9z -> ShowS
rD9z d = case c'D9'D9z d of
  Nothing -> ('0':)
  Just d9 -> rD9 d9

pGrouper :: Parser Grouper
pGrouper = ThinSpace <$ char '\x2009'
    <|> Underscore <$ char '_'

rGrouper :: Grouper -> ShowS
rGrouper g = case g of { ThinSpace -> ('\x2009':); Underscore -> ('_':) }

pRadCom :: Parser RadCom
pRadCom = Period <$ char '.'
    <|> RCGrouper <$> pGrouper

rRadCom :: RadCom -> ShowS
rRadCom r = case r of { Period -> ('.':); RCGrouper g -> rGrouper g }

pRadixRadCom :: Parser (Radix RadCom)
pRadixRadCom = Radix <$ char ','

rRadixRadCom :: Radix RadCom -> ShowS
rRadixRadCom Radix = (',':)

pRadPer :: Parser RadPer
pRadPer = Comma <$ char ','
    <|> RPGrouper <$> pGrouper

rRadPer :: RadPer -> ShowS
rRadPer c = case c of { Comma -> (',':); RPGrouper g -> rGrouper g }

pRadixRadPer :: Parser (Radix RadPer)
pRadixRadPer = Radix <$ char '.'

rRadixRadPer :: Radix RadPer -> ShowS
rRadixRadPer Radix = ('.':)

pSide :: Parser Pole
pSide = debit <$ char '<' <|> credit <$ char '>'

rSide :: Pole -> ShowS
rSide s
  | s == debit = ('<':)
  | otherwise = ('>':)

pSeqDecs :: Parser g -> Parser (Seq (g, D9z, Seq D9z))
pSeqDecs pg = pSeq ((,,) <$> pg <*> pD9z <*> pSeq pD9z)

rSeqDecs :: (g -> ShowS) -> (Seq (g, D9z, Seq D9z)) -> ShowS
rSeqDecs f = rSeq (\(g, d1, ds) -> f g . rD9z d1 . rSeq rD9z ds)

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
      <$> pD9
      <*> pSeq pD9z
      <*> pSeqDecs pg

rBG7 :: (r -> ShowS) -> BG7 r -> ShowS
rBG7 rr bg7 = case bg7 of
  BG7Zeroes z0 sz1 bg8 -> rZero z0 . rSeq rZero sz1 . rBG8 rr bg8
  BG7Novem n0 sd1 sq2 -> rD9 n0 . rSeq rD9z sd1 . rSeqDecs rr sq2

pBG8 :: Parser r -> Parser (BG8 r)
pBG8 pg = pnv <|> pgrp
  where
    pnv = BG8Novem
      <$> pD9
      <*> pSeq pD9z
      <*> pSeqDecs pg
    pgrp = BG8Group
      <$> pg
      <*> pBG7 pg

rBG8 :: (r -> ShowS) -> BG8 r -> ShowS
rBG8 rr bg8 = case bg8 of
  BG8Novem n0 sd1 sd2 -> rD9 n0 . rSeq rD9z sd1 . rSeqDecs rr sd2
  BG8Group g bg7 -> rr g . rBG7 rr bg7

pBG6 :: Parser r -> Parser (BG6 r)
pBG6 pg = pnv <|> pgrp
  where
    pnv = BG6Novem
      <$> pD9
      <*> pSeq pD9z
      <*> pg
      <*> pD9z
      <*> pSeq pD9z
      <*> pSeqDecs pg
    pgrp = BG6Group <$> pg <*> pBG7 pg

rBG6 :: (r -> ShowS) -> BG6 r -> ShowS
rBG6 rr x = case x of
  BG6Novem n0 sd1 g2 d3 sd4 sd5 ->
    rD9 n0
    . rSeq rD9z sd1
    . rr g2
    . rD9z d3
    . rSeq rD9z sd4
    . rSeqDecs rr sd5
  BG6Group r0 bg7 -> rr r0 . rBG7 rr bg7

pBG5 :: Parser r -> Parser (BG5 r)
pBG5 pg = pnv <|> pz
  where
    pnv = BG5Novem
      <$> pD9
      <*> pSeq pD9z
      <*> pg
      <*> pD9z
      <*> pSeq pD9z
      <*> pSeqDecs pg
    pz = BG5Zero
      <$> pZero
      <*> pSeq pZero
      <*> pBG6 pg

rBG5 :: (r -> ShowS) -> BG5 r -> ShowS
rBG5 rg x = case x of
  BG5Novem n0 sd1 g2 d3 sd4 sdd5 ->
    rD9 n0
    . rSeq rD9z sd1
    . rg g2
    . rD9z d3
    . rSeq rD9z sd4
    . rSeqDecs rg sdd5
  BG5Zero z0 sz0 bg6 -> rZero z0 . rSeq rZero sz0 . rBG6 rg bg6

pBG1 :: Parser (Radix r) -> Parser r -> Parser (BG1 r)
pBG1 pr pg = onLeft <|> onRight
  where
    onLeft = BG1GroupOnLeft
      <$> pg
      <*> pD9z
      <*> pSeq pD9z
      <*> pSeqDecs pg
      <*> optional
          ( (,) <$> pr <*> optional
              (((,,) <$> pD9z <*> pSeq pD9z <*> pSeqDecs pg)))

    onRight = BG1GroupOnRight
      <$> pr
      <*> pD9z
      <*> pSeq pD9z
      <*> pg <*> pD9z <*> pSeq pD9z
      <*> pSeqDecs pg

rBG1 :: (Radix r -> ShowS) -> (r -> ShowS) -> BG1 r -> ShowS
rBG1 rr rg x = case x of
  BG1GroupOnLeft r0 d1 sd2 sd3 m4
    -> rg r0
    . rD9z d1
    . rSeq rD9z sd2
    . rSeqDecs rg sd3
    . case m4 of
        Nothing -> id
        Just (r5, m6) -> rr r5 . case m6 of
          Nothing -> id
          Just (d7, sd8, sd9) -> rD9z d7 . rSeq rD9z sd8
            . rSeqDecs rg sd9
  BG1GroupOnRight r0 d1 sd2 g3 d4 ds5 sd6 ->
    rr r0 . rD9z d1 . rSeq rD9z sd2
    . rg g3 . rD9z d4 . rSeq rD9z ds5 . rSeqDecs rg sd6

pBrimUngrouped :: Parser (Radix r) -> Parser (BrimUngrouped r)
pBrimUngrouped pr = gtOne <|> ltOne
  where
    gtOne = BUGreaterThanOne
      <$> pD9
      <*> pSeq pD9z
      <*> optional ((,) <$> pr <*> pSeq pD9z)
    ltOne = BULessThanOne
      <$> optional pZero
      <*> pr
      <*> pSeq pZero
      <*> pD9
      <*> pSeq pD9z

rBrimUngrouped :: (Radix r -> ShowS) -> BrimUngrouped r -> ShowS
rBrimUngrouped rr x = case x of
  BUGreaterThanOne n0 sd1 m2 -> rD9 n0 . rSeq rD9z sd1
    . case m2 of
        Nothing -> id
        Just (r3, sd4) -> rr r3 . rSeq rD9z sd4
  BULessThanOne mz0 r1 sz2 n3 sd4 -> rMaybe rZero mz0
    . rr r1 . rSeq rZero sz2 . rD9 n3 . rSeq rD9z sd4

pBrimGrouped :: Parser (Radix r) -> Parser r -> Parser (BrimGrouped r)
pBrimGrouped pr pg = gtOne <|> ltOne
  where
    gtOne = BGGreaterThanOne
      <$> pD9
      <*> pSeq pD9z
      <*> pBG1 pr pg
    ltOne = BGLessThanOne
      <$> optional pZero
      <*> pr
      <*> pBG5 pg

rBrimGrouped :: (Radix r -> ShowS) -> (r -> ShowS) -> BrimGrouped r -> ShowS
rBrimGrouped rr rg x = case x of
  BGGreaterThanOne n0 sd1 bg1 -> rD9 n0 . rSeq rD9z sd1 . rBG1 rr rg bg1
  BGLessThanOne mz0 r1 bg5 -> rMaybe rZero mz0
    . rr r1 . rBG5 rg bg5

-- | If fails, does not consume any input.
pBrim :: Parser (Radix r) -> Parser r -> Parser (Brim r)
pBrim pr pg = BrimGrouped <$> try (pBrimGrouped pr pg)
    <|> BrimUngrouped <$> try (pBrimUngrouped pr)

rBrim :: (Radix r -> ShowS) -> (r -> ShowS) -> Brim r -> ShowS
rBrim rr rg x = case x of
  BrimGrouped bg -> rBrimGrouped rr rg bg
  BrimUngrouped bu -> rBrimUngrouped rr bu

-- | If fails, does not consume any input.
pNil :: Parser (Radix r) -> Parser r -> Parser (Nil r)
pNil pr pg = NilU <$> try (pNilUngrouped pr)
    <|> NilG <$> try (pNilGrouped pr pg)

rNil :: (Radix r -> ShowS) -> (r -> ShowS) -> Nil r -> ShowS
rNil rr rg x = case x of
  NilU nu -> rNilUngrouped rr nu
  NilG ng -> rNilGrouped rr rg ng

-- | If fails, does not consume any input.
pNilOrBrimScalar
  :: Parser (Radix r)
  -> Parser r
  -> Parser (NilOrBrimScalar r)
pNilOrBrimScalar pr pg = Left <$> pNil pr pg
    <|> Right <$> pBrim pr pg

rNilOrBrimScalar
  :: (Radix r -> ShowS)
  -> (r -> ShowS)
  -> NilOrBrimScalar r
  -> ShowS
rNilOrBrimScalar rr rg ei = case ei of
  Left n -> rNil rr rg n
  Right b -> rBrim rr rg b

pPluMin :: Parser PluMin
pPluMin = Plus <$ char '+' <|> Minus <$ char '-'

rPluMin :: PluMin -> ShowS
rPluMin Plus = ('+':)
rPluMin Minus = ('-':)


