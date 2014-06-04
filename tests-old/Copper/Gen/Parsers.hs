{-# LANGUAGE OverloadedStrings #-}
module Copper.Gen.Parsers where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow (first)
import qualified Control.Monad.Trans.Writer as W
import Control.Monad.Trans.Class (lift)
import Data.List (nubBy, intersperse)
import qualified Penny.Lincoln as L
import qualified Penny.Copper as C
import qualified Penny.Copper.Render as CR
import qualified Data.Time as Time
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Sums as S

import qualified System.Random.Shuffle as Shuffle
import qualified Lincoln as TL
import qualified Copper.Gen.Terminals as T
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as X
import Data.Text (pack, snoc, cons)
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck as Q
import Test.QuickCheck (arbitrary, suchThat)

--
-- * Helpers
--

optional :: Gen (a, X.Text) -> Gen (Maybe a, X.Text)
optional g = do
  b <- arbitrary
  if b
    then fmap (first Just) g
    else return (Nothing, X.empty)

interleave :: Gen (Maybe a) -> [a] -> Gen [a]
interleave g ls =
  let withGens = intersperse g (map (return . Just) ls)
  in fmap catMaybes . sequence $ withGens

leadingZero :: Show s => s -> X.Text
leadingZero s =
  let str = show s
  in if length str > 1
      then pack str
      else pack ('0':str)

surround
  :: Char -> Char -> (X.Text -> a) -> Gen Char -> Gen (a, X.Text)
surround o c m g = do
  x <- fmap pack $ G.listOf g
  return (m x, o `cons` x `snoc` c)

shuffle :: [a] -> Gen [a]
shuffle [] = return []
shuffle ls = G.MkGen $ \g _ -> Shuffle.shuffle' ls (length ls) g

white :: Gen X.Text
white = fmap pack (G.listOf T.white)

--
-- * Accounts
--

lvl1SubAcct :: Gen (L.SubAccount, X.Text)
lvl1SubAcct = do
  txt <- fmap pack $ G.listOf1 T.lvl1AcctChar
  return (L.SubAccount txt, txt)

lvl1FirstSubAcct :: Gen (L.SubAccount, X.Text)
lvl1FirstSubAcct = lvl1SubAcct

lvl1OtherSubAcct :: Gen (L.SubAccount, X.Text)
lvl1OtherSubAcct = do
  (ac, gtxt) <- lvl1SubAcct
  return (ac, ':' `cons` gtxt)

lvl1Acct :: Gen (L.Account, X.Text)
lvl1Acct = do
  s1 <- lvl1SubAcct
  sr <- G.listOf lvl1OtherSubAcct
  let subs = fst s1 : fmap fst sr
      txts = X.concat . map snd $ s1:sr
  return (L.Account subs, txts)

quotedLvl1Acct :: Gen (L.Account, X.Text)
quotedLvl1Acct = fmap f lvl1Acct
  where
    f (ac, gTxt) = (ac, '{' `cons` gTxt `snoc` '}')

lvl2FirstSubAcct :: Gen (L.SubAccount, X.Text)
lvl2FirstSubAcct = do
  l1 <- T.letter
  ls <- fmap pack $ G.listOf T.lvl2AcctOtherChar
  let txt = l1 `cons` ls
  return (L.SubAccount txt, txt)

lvl2OtherSubAcct :: Gen (L.SubAccount, X.Text)
lvl2OtherSubAcct = do
  cs <- fmap pack $ G.listOf1 T.lvl2AcctOtherChar
  return (L.SubAccount cs, ':' `cons` cs)

lvl2Acct :: Gen (L.Account, X.Text)
lvl2Acct = do
  a1 <- lvl2FirstSubAcct
  as <- G.listOf lvl2OtherSubAcct
  let a = L.Account (map fst (a1:as))
      g = X.concat . map snd $ (a1:as)
  return (a, g)

ledgerAcct :: Gen (L.Account, X.Text)
ledgerAcct = G.oneof [quotedLvl1Acct, lvl2Acct]

--
-- * Commodities
--

data QuotedLvl1Cmdty = QuotedLvl1Cmdty L.Commodity X.Text
  deriving Show

data Lvl2Cmdty = Lvl2Cmdty L.Commodity X.Text
  deriving Show

data Lvl3Cmdty = Lvl3Cmdty L.Commodity X.Text
  deriving Show

data Cmdty
  = L1 QuotedLvl1Cmdty
  | L2 Lvl2Cmdty
  | L3 Lvl3Cmdty
  deriving Show

unwrapCmdty :: Cmdty -> (L.Commodity, X.Text)
unwrapCmdty c = case c of
  L1 (QuotedLvl1Cmdty y x) -> (y, x)
  L2 (Lvl2Cmdty y x) -> (y, x)
  L3 (Lvl3Cmdty y x) -> (y, x)

lvl1Cmdty :: Gen (L.Commodity, X.Text)
lvl1Cmdty = do
  cs <- fmap X.pack (G.listOf1 T.lvl1CmdtyChar)
  return (L.Commodity cs, cs)

quotedLvl1Cmdty :: Gen QuotedLvl1Cmdty
quotedLvl1Cmdty = fmap f lvl1Cmdty
  where
    f (c, gx) = QuotedLvl1Cmdty c ('"' `cons` gx `snoc` '"')

lvl2Cmdty :: Gen Lvl2Cmdty
lvl2Cmdty = do
  c1 <- T.lvl2CmdtyFirstChar
  cs <- fmap pack $ G.listOf T.lvl2CmdtyOtherChar
  return (Lvl2Cmdty (L.Commodity $ c1 `cons` cs) (c1 `cons` cs))

lvl3Cmdty :: Gen Lvl3Cmdty
lvl3Cmdty = do
  cs <- fmap pack $ G.listOf1 T.lvl3CmdtyChar
  return (Lvl3Cmdty (L.Commodity cs) cs)

genCmdty :: Gen Cmdty
genCmdty = G.oneof [ fmap L1 quotedLvl1Cmdty
                   , fmap L2 lvl2Cmdty
                   , fmap L3 lvl3Cmdty ]

uniqueCmdtys :: Gen [Cmdty]
uniqueCmdtys = G.sized $ \s -> do
  ls <- (G.resize (min s 3) $ G.listOf1 genCmdty)
  let f c1 c2 = (fst . unwrapCmdty $ c1) == (fst . unwrapCmdty $ c2)
  return (nubBy f ls)


--
-- * Quantities
--


--
-- ## QtyRep
--

type Signif = Integer
type Places = Integer

class Ast a where
  ast :: Gen (a, X.Text)

instance Ast L.Digit where
  ast = Q.elements . map (\(d, c) -> (d, X.singleton c)) $
    zip [L.D0 .. L.D9] ['0'..'9']

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty g = (:|) <$> g <*> Q.listOf g

instance Ast L.DigitList where
  ast = fmap f (genNonEmpty ast)
    where
      f ls = ( L.DigitList . fmap fst $ ls
             , X.concat . NE.toList . fmap snd $ ls)

instance Ast L.PeriodGrp where
  ast
    = fmap f
    . Q.elements
    $ [(' ', L.PGSpace), ('\x2009', L.PGThinSpace), (',', L.PGComma)]
    where
      f (c, e) = (e, X.singleton c)

instance Ast L.CommaGrp where
  ast = fmap f . Q.elements
    $ [(' ', L.CGSpace), ('\x2009', L.CGThinSpace), ('.', L.CGPeriod)]
    where
      f (c, e) = (e, X.singleton c)

instance Ast a => Ast (L.GroupedDigits a) where
  ast = do
    (d1a, d1x) <- ast
    (dsa, dsx) <- fmap unzip $ Q.listOf ast
    (ssa, ssx) <- fmap unzip $ Q.vectorOf (length dsa) ast
    let ra = L.GroupedDigits d1a (zip ssa dsa)
        rx = d1x <> (X.concat (map toTxt (zip ssx dsx)))
        toTxt (x1, x2) = x1 <> x2
    return (ra, rx)


wholeFrac
  :: (L.Digits a, Ast a)
  => L.Radix
  -> Gen (L.WholeFrac a, X.Text)
wholeFrac rad = do
  let hasNonZero (x, y) =
        TL.digitsHasNonZero (fst x) || TL.digitsHasNonZero (fst y)
  ((wa, wx), (fa, fx)) <- ((,) <$> ast <*> ast) `suchThat` hasNonZero
  let r = fromMaybe (error "wholeFracDigits: wholeFrac failed")
        (L.wholeFrac wa fa)
      rx = wx <> repRadix rad <> fx
  return (r, rx)

instance (Ast a, L.Digits a) => Ast (L.WholeOnly a) where
  ast = do
    (wa, wx) <- ast `suchThat` (TL.digitsHasNonZero . fst)
    let r = fromMaybe (error "generating WholeOnly failed")
                      (L.wholeOnly wa)
    return (r, wx)

wholeOrFrac
  :: (L.Digits a, Ast a)
  => L.Radix
  -> Gen (L.WholeOrFrac a, X.Text)
wholeOrFrac r = do
  left <- arbitrary
  if left
    then do
      (w, x) <- ast
      return (L.WholeOrFrac . Left $ w, x)
    else do
      (w, x) <- wholeFrac r
      return (L.WholeOrFrac . Right $ w, x)

instance Ast L.QtyRep where
  ast = do
    (qr, x) <- do
      grouped <- arbitrary
      if grouped
        then do
          left <- arbitrary
          if left
            then do
              (wf, x) <- wholeOrFrac L.Period
              return (L.QGrouped (Left wf), x)
            else do
              (wf, x) <- wholeOrFrac L.Comma
              return (L.QGrouped (Right wf), x)
        else do
          rx <- arbitrary
          (wf, x) <- wholeOrFrac rx
          return (L.QNoGrouping wf rx, x)
    let (b, e) = CR.quoteQtyRep qr
    return (qr, b <> x <> e)

repRadix :: L.Radix -> X.Text
repRadix r = X.singleton $ case r of
  L.Comma -> ','
  L.Period -> '.'


-- | Generate a random representation for a Qty.
representQty :: L.Qty -> Gen L.QtyRep
representQty q = L.qtyToRep <$> arbitrary <*> pure q

renderQty :: L.Qty -> Gen (L.QtyRep, X.Text)
renderQty q = do
  qr <- representQty q
  return (qr, CR.qtyRep qr)


--
-- * Amounts
--

spaceBetween :: X.Text -> L.SpaceBetween
spaceBetween x = if X.null x then L.NoSpaceBetween else L.SpaceBetween

leftCmdtyLvl1Amt
  :: QuotedLvl1Cmdty
  -> (L.QtyRep, X.Text)
  -> Gen ((L.Amount L.QtyRep, X.Text), L.SpaceBetween)
leftCmdtyLvl1Amt (QuotedLvl1Cmdty c xc) (q, xq) = do
  ws <- white
  let amt = L.Amount q c
  return ((amt, X.concat [xc, ws, xq]), spaceBetween ws)


leftCmdtyLvl3Amt
  :: Lvl3Cmdty
  -> (L.QtyRep, X.Text)
  -> Gen ((L.Amount L.QtyRep, X.Text), L.SpaceBetween)
leftCmdtyLvl3Amt (Lvl3Cmdty c xc) (q, xq) = do
  ws <- white
  let amt = L.Amount q c
  return ((amt, X.concat [xc, ws, xq]), spaceBetween ws)


leftSideCmdtyAmt
  :: Either QuotedLvl1Cmdty Lvl3Cmdty
  -> (L.QtyRep, X.Text)
  -> Gen ((L.Amount L.QtyRep, X.Text), L.SpaceBetween)
leftSideCmdtyAmt c q = case c of
  Left l1 -> leftCmdtyLvl1Amt l1 q
  Right l3 -> leftCmdtyLvl3Amt l3 q

rightSideCmdty :: Gen (Either QuotedLvl1Cmdty Lvl2Cmdty)
rightSideCmdty = G.oneof
  [ fmap Left quotedLvl1Cmdty
  , fmap Right lvl2Cmdty ]


rightSideCmdtyAmt
  :: Either QuotedLvl1Cmdty Lvl2Cmdty
  -> (L.QtyRep, X.Text)
  -> Gen ((L.Amount L.QtyRep, X.Text), L.SpaceBetween)
rightSideCmdtyAmt cty (q, xq) = do
  ws <- white
  let (c, xc) = case cty of
        Left (QuotedLvl1Cmdty ct x) -> (ct, x)
        Right (Lvl2Cmdty ct x) -> (ct, x)
      xr = X.concat [xq, ws, xc]
      amt = L.Amount q c
  return ((amt, xr), spaceBetween ws)


amount
  :: Cmdty
  -> (L.QtyRep, X.Text)
  -> Gen ((L.Amount L.QtyRep, X.Text), L.SpaceBetween, L.Side)
amount c q =
  let mkTrip sd (p, b) = (p, b, sd)
  in case c of
      L1 l1 -> G.oneof
        [ fmap (mkTrip L.CommodityOnLeft) (leftSideCmdtyAmt (Left l1) q)
        , fmap (mkTrip L.CommodityOnRight) (rightSideCmdtyAmt (Left l1) q)
        ]
      L2 l2 -> fmap (mkTrip L.CommodityOnRight)
                    (rightSideCmdtyAmt (Right l2) q)
      L3 l3 -> fmap (mkTrip L.CommodityOnLeft)
                    (leftSideCmdtyAmt (Right l3) q)

--
-- * Comments
--

comment :: Gen (C.Comment, X.Text)
comment = do
  x <- fmap pack $ G.listOf T.nonNewline
  ws <- white
  let txt = ('#' `cons` x `snoc` '\n') `X.append` ws
  return (C.Comment x, txt)

--
-- * DateTime
--

year :: Gen (Integer, X.Text)
year = do
  i <- G.choose (1492, 2400)
  return (i, pack . show $ i)

month :: Gen (Int, X.Text)
month = do
  i <- G.choose (1, 12)
  return (i, leadingZero i)


day :: Gen (Int, X.Text)
day = do
  i <- G.choose (1, 28)
  return (i, leadingZero i)

throwMaybe :: Monad m => String -> Maybe a -> m a
throwMaybe s = maybe (fail $ "could not generate " ++ s) return

date :: Gen (Time.Day, X.Text)
date = do
  (y, yTxt) <- year
  (m, mTxt) <- month
  (d, dTxt) <- day
  dt <- throwMaybe "date" $ Time.fromGregorianValid y m d
  s1 <- T.dateSep
  s2 <- T.dateSep
  let x = (yTxt `snoc` s1) `X.append` (mTxt `snoc` s2) `X.append` dTxt
  return (dt, x)


hours :: Gen (L.Hours, X.Text)
hours =  do
  h <- G.choose (0, 23)
  hr <- throwMaybe "hours" (L.intToHours h)
  let x = leadingZero h
  return (hr, x)

minutes :: Gen (L.Minutes, X.Text)
minutes = do
  m <- G.choose (0, 59)
  mi <- throwMaybe "minutes" (L.intToMinutes m)
  return (mi, ':' `cons` leadingZero m)

seconds :: Gen (L.Seconds, X.Text)
seconds = do
  s <- G.choose (0, 59)
  let _types = s :: Int
  se <- throwMaybe "seconds" (L.intToSeconds (fromIntegral s))
  return (se, ':' `cons` leadingZero s)


time :: Gen ((L.Hours, L.Minutes, Maybe L.Seconds), X.Text)
time = do
  (h, ht) <- hours
  (m, mt) <- minutes
  (s, st) <- optional seconds
  let x = ht `X.append` mt `X.append` st
  return ((h, m, s), x)

tzSign :: Gen (Int -> Int, X.Text)
tzSign = do
  s <- Q.arbitrary
  return $ if s
    then (id, X.singleton '+')
    else (negate, X.singleton '-')

tzNumber :: Gen (Int, X.Text)
tzNumber = do
  i <- G.choose (0, 840)
  return (i, X.justifyRight 4 '0' (pack . show $ i))

timeZone :: Gen (L.TimeZoneOffset, X.Text)
timeZone = do
  (s, st) <- tzSign
  (n, nt) <- tzNumber
  o <- throwMaybe "time zone" (L.minsToOffset (s n))
  return (o, st `X.append` nt)

timeWithZone ::
  Gen
  ((L.Hours, L.Minutes, Maybe L.Seconds, Maybe L.TimeZoneOffset), X.Text)
timeWithZone = do
  ((h, m, mays), xt) <- time
  ws <- white
  (o, xo) <- optional timeZone
  let x = xt `X.append` ws `X.append` xo
  return ((h, m, mays, o), x)

dateTime :: Gen (L.DateTime, X.Text)
dateTime = do
  (d, xd) <- date
  w <- white
  (t, xt) <- optional timeWithZone
  let ((h, m, s), tz) = case t of
        Nothing -> (L.midnight, L.noOffset)
        Just (hr, mn, mayS, mayTz) ->
          let sec = fromMaybe L.zeroSeconds mayS
              z = fromMaybe L.noOffset mayTz
          in ((hr, mn, sec), z)
  return (L.DateTime d h m s tz, xd `X.append` w `X.append` xt)

--
-- * DrCr
--


debit :: Gen (L.DrCr, X.Text)
debit = (\x -> (L.Debit, X.singleton x)) <$> T.lessThan

credit :: Gen (L.DrCr, X.Text)
credit = (\x -> (L.Credit, X.singleton x)) <$> T.greaterThan

drCr :: Gen (L.DrCr, X.Text)
drCr = G.oneof [debit, credit]

--
-- * Entry
--


entry
  :: Cmdty
  -> (L.DrCr, X.Text)
  -> (L.QtyRep, X.Text)
  -> Gen ((L.Entry L.QtyRep, X.Text), L.SpaceBetween, L.Side)
entry c (d, xd) q = f <$> white <*> amount c q
  where
    f w ((a, xa), sb, sd) = ((L.Entry d a, x), sb, sd)
      where
        x = X.concat [xd, w, xa]


genEntryGroup
  :: Cmdty
  -> Gen [((L.Entry L.QtyRep, X.Text), L.SpaceBetween, L.Side)]
genEntryGroup c = do
  dr <- debit
  cr <- credit
  (_, dq, cq) <- TL.genBalQtys
  dqWithX <- mapM renderQty dq
  cqWithX <- mapM renderQty cq
  des <- mapM (entry c dr) dqWithX
  ces <- mapM (entry c cr) cqWithX
  return $ des ++ ces


genEntryGroups
  :: Gen [((L.Entry L.QtyRep, X.Text), L.SpaceBetween, L.Side)]
genEntryGroups = do
  cs <- uniqueCmdtys
  fmap concat . mapM genEntryGroup $ cs


--
-- * Flag
--

flag :: Gen (L.Flag, X.Text)
flag = surround '[' ']' L.Flag T.flagChar


--
-- * Memo
--

postingMemoLine :: Gen (X.Text, X.Text)
postingMemoLine = do
  me <- fmap pack $ G.listOf T.nonNewline
  ws <- white
  return (me, '\'' `cons` me `snoc` '\n' `X.append` ws)

postingMemo :: Gen (L.Memo, X.Text)
postingMemo = do
  me <- G.listOf1 postingMemoLine
  let mem = L.Memo . map fst $ me
      ren = X.concat .  map snd $ me
  return (mem, ren)

transactionMemoLine :: Gen (X.Text, X.Text)
transactionMemoLine = do
  me <- fmap pack $ G.listOf T.nonNewline
  ws <- white
  return (me, ';' `cons` me `snoc` '\n' `X.append` ws)

transactionMemo :: Gen (L.Memo, X.Text)
transactionMemo = do
  me <- G.listOf1 transactionMemoLine
  let mem = L.Memo . map fst $ me
      ren = X.concat . map snd $ me
  return (mem, ren)

--
-- * Number
--

number :: Gen (L.Number, X.Text)
number = surround '(' ')' L.Number T.numberChar

--
-- * Payee
--

lvl1Payee :: Gen (L.Payee, X.Text)
lvl1Payee = do
  x <- fmap pack $ G.listOf T.quotedPayeeChar
  return (L.Payee x, x)

quotedLvl1Payee :: Gen (L.Payee, X.Text)
quotedLvl1Payee = do
  (p, xp) <- lvl1Payee
  return (p, '~' `cons` xp `snoc` '~')

lvl2Payee :: Gen (L.Payee, X.Text)
lvl2Payee = do
  l1 <- T.letter
  ls <- fmap pack $ G.listOf T.nonNewline
  let x = l1 `cons` ls
  return (L.Payee x, x)


--
-- * Price
--


fromCmdty
  :: Either QuotedLvl1Cmdty Lvl2Cmdty
  -> (L.From, X.Text)
fromCmdty e = case e of
  Left (QuotedLvl1Cmdty c x) -> (L.From c, x)
  Right (Lvl2Cmdty c x) -> (L.From c, x)


price :: Gen (L.PricePoint, X.Text)
price = do
  atSign <- T.atSign
  wsAt <- white
  fr <- G.oneof [ fmap Left lvl3Cmdty, fmap Right quotedLvl1Cmdty ]
  (dt, xdt) <- dateTime
  ws1 <- white
  let (fc, xfc) = case fr of
        Left (Lvl3Cmdty c x) -> (c, x)
        Right (QuotedLvl1Cmdty c x) -> (c, x)
  ws2 <- fmap pack (G.listOf1 T.white)
  qty <- arbitrary
  q <- renderQty qty
  let pdct x = (fst . unwrapCmdty $ x) /= fc
  toCmdty <- Q.suchThat genCmdty pdct
  (((L.Amount toQ t), xam), sb, sd) <- amount toCmdty q
  let (to, cpu) = (L.To t, L.CountPerUnit toQ)
  p <- throwMaybe "price" (L.newPrice (L.From fc) to cpu)
  ws3 <- white
  let pp = L.PricePoint dt p (Just sd) (Just sb) Nothing
      x = X.concat [ X.singleton atSign, wsAt, xdt, ws1, xfc,
                     ws2, xam, X.singleton '\n', ws3]
  return (pp, x)


--
-- * Tags
--

tag :: Gen (L.Tag, X.Text)
tag = do
  x <- fmap pack $ G.listOf T.tagChar
  w <- white
  return (L.Tag x, '*' `cons` x `X.append` w)

tags :: Gen (L.Tags, X.Text)
tags = fmap f $ G.listOf1 tag
  where
    f ls = (L.Tags . map fst $ ls, X.concat . map snd $ ls)

--
-- * TopLine
--

topLinePayee :: Gen (L.Payee, X.Text)
topLinePayee = G.oneof [quotedLvl1Payee, lvl2Payee]

topLineFlagNum :: Gen ((Maybe L.Flag, Maybe L.Number), X.Text)
topLineFlagNum = f <$> optional flag <*> white <*> optional number
  where
    f (fl, xfl) ws (nu, xnu) = ((fl, nu), X.concat [xfl, ws, xnu])


topLineCore :: Gen (L.TopLineCore, X.Text)
topLineCore = do
  (me, xme) <- optional transactionMemo
  (dt, xdt) <- dateTime
  w1 <- white
  ((fl, nu), xfn) <- topLineFlagNum
  w2 <- white
  (pa, xp) <- optional topLinePayee
  w3 <- white
  let tl = L.TopLineCore dt nu fl pa me
      x = X.concat [xme, xdt, w1, xfn, w2, xp, X.singleton '\n', w3]
  return (tl, x)


--
-- * Posting
--


liftMaybeGen :: Gen (a, X.Text) -> W.WriterT [X.Text] Gen (Maybe a)
liftMaybeGen g = do
  b <- lift arbitrary
  if b
    then do
      (r, x) <- lift g
      ws <- lift white
      W.tell [X.append x ws]
      return (Just r)
    else return Nothing

flagNumPayee
  :: Gen ((Maybe L.Flag, Maybe L.Number, Maybe L.Payee), X.Text)
flagNumPayee = do
  (r, ls) <- W.runWriterT
             $ (,,)
             <$> liftMaybeGen flag
             <*> liftMaybeGen number
             <*> liftMaybeGen quotedLvl1Payee
  ls' <- shuffle ls
  return (r, X.concat ls')

postingAcct :: Gen (L.Account, X.Text)
postingAcct = G.oneof [quotedLvl1Acct, lvl2Acct]


posting
 :: Maybe ((L.Entry L.QtyRep, X.Text), L.SpaceBetween, L.Side)
 -> Gen ((L.PostingCore, X.Text), Maybe (L.Entry L.QtyRep))
posting mayEn = do
  (mayFnp, xfnp) <- optional flagNumPayee
  let (fl, nu, pa) = fromMaybe (Nothing, Nothing, Nothing) mayFnp
  ws1 <- white
  (ac, xa) <- postingAcct
  ws2 <- white
  doTags <- Q.arbitrary
  (ts, xt) <- if doTags
                then tags
                else return (L.Tags [], X.empty)
  doMemo <- Q.arbitrary
  (pm, xm) <- if doMemo
                then do
                      (mo, xMo) <- postingMemo
                      return (Just mo, xMo)
                else return (Nothing, X.empty)
  ws3 <- white
  ws4 <- white
  ws5 <- white
  let (en, xe, sd, sb) = case mayEn of
        Nothing -> (Nothing, X.empty, Nothing, Nothing)
        Just ((jEn, jXe), jSd, jSb) -> (Just jEn, jXe, Just jSd, Just jSb)
      po = L.PostingCore pa nu fl ac ts pm sb sd
      txt = X.concat
            [ xfnp, ws1, xa, ws2, xt, ws3, xe,
              X.singleton '\n', ws4, xm, ws5]
  return ((po, txt), en)


postings :: Gen [((L.PostingCore, X.Text), Maybe (L.Entry L.QtyRep))]
postings = do
  egs <- genEntryGroups
  removeEn <- Q.arbitrary
  ps <- if removeEn
        then do
          p1 <- posting Nothing
          ps <- mapM posting . map Just . tail $ egs
          return (p1:ps)
        else mapM posting . map Just $ egs
  shuffle ps


--
-- * Transaction
--


transaction :: Gen ((L.TopLineCore, L.Ents L.PostingCore), X.Text)
transaction = do
  (tl, xtl) <- topLineCore
  pstgs <- postings
  let x = xtl `X.append` (X.concat . map (snd . fst) $ pstgs)
      es = map (\((pc, _), mayEn) -> (fmap Left mayEn, pc)) pstgs
      _types = es :: [ ( Maybe (Either (L.Entry L.QtyRep) a)
                       , L.PostingCore)]
  case L.ents es of
    Nothing -> fail $ "failed to create transaction: " ++ show pstgs
    Just r -> return ((tl, r), x)


--
-- * BlankLine
--

blankLine :: Gen (C.BlankLine, X.Text)
blankLine = fmap f white
  where
    f ws = (C.BlankLine, '\n' `cons` ws)


--
-- * Item
--


type TestItem
  = S.S4 (L.TopLineCore, L.Ents L.PostingCore)
         L.PricePoint
         C.Comment
         C.BlankLine

item :: Gen (TestItem, X.Text)
item = Q.oneof
  [ fmap (\(c, x) -> (S.S4c c, x)) comment
  , fmap (\(p, x) -> (S.S4b p, x)) price
  , fmap (\(t, x) -> (S.S4a t, x)) transaction
  , fmap (\(b, x) -> (S.S4d b, x)) blankLine
  ]

--
-- * Ledger
--

ledger :: Gen ([TestItem], X.Text)
ledger = f <$> white <*> Q.listOf item
  where
    f ws is = (map fst is
              , ws `X.append` (X.concat . map snd $ is))



