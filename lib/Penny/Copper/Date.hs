{-# LANGUAGE FlexibleContexts #-}
module Penny.Copper.Date where

import Data.Time
import Penny.Copper.Classes
-- import Penny.Copper.Parser
import Text.ParserCombinators.UU.BasicInstances -- hiding (Parser)
import Control.Applicative
import Penny.Copper.LincolnTypes
import Penny.Lincoln

data DateSep = Slash | Hyphen
  deriving (Eq, Ord, Show)

pDateSep :: Parser DateSep
pDateSep = Slash <$ pSym '/' <|> Hyphen <$ pSym '-'

rDateSep :: DateSep -> ShowS
rDateSep x = case x of { Slash -> ('/':); Hyphen -> ('-':) }

instance Parseable DateSep where parser = pDateSep
instance Renderable DateSep where render = rDateSep

data One = One
  deriving (Eq, Ord, Show)

pOne :: Parser One
pOne = One <$ pSym '1'

rOne :: One -> ShowS
rOne One = ('1':)

instance Parseable One where parser = pOne
instance Renderable One where render = rOne

data Two = Two
  deriving (Eq, Ord, Show)

instance Parseable Two where parser = pTwo
instance Renderable Two where render = rTwo

pTwo :: Parser Two
pTwo = Two <$ pSym '2'

rTwo :: Two -> ShowS
rTwo Two = ('2':)

data Three = Three
  deriving (Eq, Ord, Show)

pThree :: Parser Three
pThree = Three <$ pSym '3'

rThree :: Three -> ShowS
rThree Three = ('3':)

instance Parseable Three where parser = pThree
instance Renderable Three where render = rThree

data Four = Four
  deriving (Eq, Ord, Show)

pFour :: Parser Four
pFour = Four <$ pSym '4'

rFour :: Four -> ShowS
rFour Four = ('4':)

instance Parseable Four where parser = pFour
instance Renderable Four where render = rFour

data Five = Five
  deriving (Eq, Ord, Show)

pFive :: Parser Five
pFive = Five <$ pSym '5'

rFive :: Five -> ShowS
rFive Five = ('5':)

instance Parseable Five where parser = pFive
instance Renderable Five where render = rFive

data Six = Six
  deriving (Eq, Ord, Show)

pSix :: Parser Six
pSix = Six <$ pSym '6'

rSix :: Six -> ShowS
rSix Six = ('6':)

instance Parseable Six where parser = pSix
instance Renderable Six where render = rSix

data Seven = Seven
  deriving (Eq, Ord, Show)

pSeven :: Parser Seven
pSeven = Seven <$ pSym '7'

rSeven :: Seven -> ShowS
rSeven Seven = ('7':)

instance Parseable Seven where parser = pSeven
instance Renderable Seven where render = rSeven

data Eight = Eight
  deriving (Eq, Ord, Show)

pEight :: Parser Eight
pEight = Eight <$ pSym '8'

rEight :: Eight -> ShowS
rEight Eight = ('8':)

instance Parseable Eight where parser = pEight
instance Renderable Eight where render = rEight

data Nine = Nine
  deriving (Eq, Ord, Show)

pNine :: Parser Nine
pNine = Nine <$ pSym '9'

rNine :: Nine -> ShowS
rNine Nine = ('9':)

instance Parseable Nine where parser = pNine
instance Renderable Nine where render = rNine

data Days28
  = D28'1to9 D0z D9
  | D28'10to19 One D9z
  | D28'20to28 Two D8
  deriving (Eq, Ord, Show)

c'Days28'Int :: Integral a => a -> Maybe Days28
c'Days28'Int a
  | a > 0 && a < 10 = do
      d <- intToDigit a
      return $ D28'1to9 D0z'0 d
  | a >= 10 && a < 20 = do
      d <- intToDigit (a - 10)
      return $ D28'10to19 One d
  | a >= 20 && a < 29 = do
      d <- intToDigit (a - 20)
      return $ D28'20to28 Two d
  | otherwise = Nothing

c'Int'Days28 :: Integral a => Days28 -> a
c'Int'Days28 day = case day of
  D28'1to9 _ d9 -> digitToInt d9
  D28'10to19 _ d9z -> digitToInt d9z + 10
  D28'20to28 _ d8 -> digitToInt d8 + 20

pDays28 :: Parser Days28
pDays28
  = D28'1to9 <$> pD0z <*> pD9
  <|> D28'10to19 <$> pOne <*> pD9z
  <|> D28'20to28 <$> pTwo <*> pD8

rDays28 :: Days28 -> ShowS
rDays28 x = case x of
  D28'1to9 d0 d9 -> rD0z d0 . rD9 d9
  D28'10to19 o d9z -> rOne o . rD9z d9z
  D28'20to28 t d8 -> rTwo t . rD8 d8

instance Parseable Days28 where parser = pDays28
instance Renderable Days28 where render = rDays28

data Days30
  = D30'28 Days28
  | D30'29 Two Nine
  | D30'30 Three D0z
  deriving (Eq, Ord, Show)

c'Days30'Int :: Integral a => a -> Maybe Days30
c'Days30'Int a
  | a == 29 = Just $ D30'29 Two Nine
  | a == 30 = Just $ D30'30 Three D0z'0
  | otherwise = fmap D30'28 $ c'Days28'Int a

c'Int'Days30 :: Integral a => Days30 -> a
c'Int'Days30 days = case days of
  D30'28 d28 -> c'Int'Days28 d28
  D30'29 _ _ -> 29
  D30'30 _ _ -> 30

pDays30 :: Parser Days30
pDays30
  = D30'28 <$> pDays28
  <|> D30'29 <$> pTwo <*> pNine
  <|> D30'30 <$> pThree <*> pD0z

rDays30 :: Days30 -> ShowS
rDays30 x = case x of
  D30'28 d -> rDays28 d
  D30'29 t n -> rTwo t . rNine n
  D30'30 t o -> rThree t . rD0z o

instance Parseable Days30 where parser = pDays30
instance Renderable Days30 where render = rDays30

data Days31
  = D31'30 Days30
  | D31'31 Three One
  deriving (Eq, Ord, Show)

instance Parseable Days31 where parser = pDays31
instance Renderable Days31 where render = rDays31

c'Days31'Int :: Integral a => a -> Maybe Days31
c'Days31'Int a
  | a == 31 = Just $ D31'31 Three One
  | otherwise = fmap D31'30 $ c'Days30'Int a

c'Int'Days31 :: Integral a => Days31 -> a
c'Int'Days31 days31 = case days31 of
  D31'30 d30 -> c'Int'Days30 d30
  D31'31 _ _ -> 31

pDays31 :: Parser Days31
pDays31
  = D31'30 <$> pDays30
  <|> D31'31 <$> pThree <*> pOne

rDays31 :: Days31 -> ShowS
rDays31 x = case x of
  D31'30 d -> rDays30 d
  D31'31 t o -> rThree t . rOne o

data MonthDay
  = Jan D0z One       DateSep Days31
  | Feb D0z Two       DateSep Days28
  | Mar D0z Three     DateSep Days31
  | Apr D0z Four      DateSep Days30
  | May D0z Five      DateSep Days31
  | Jun D0z Six       DateSep Days30
  | Jul D0z Seven     DateSep Days31
  | Aug D0z Eight     DateSep Days31
  | Sep D0z Nine      DateSep Days30
  | Oct One D0z       DateSep Days31
  | Nov One One       DateSep Days30
  | Dec One Two       DateSep Days31
  deriving (Eq, Ord, Show)

c'MonthDay'Ints
  :: (Integral a, Integral b)
  => DateSep
  -> a
  -- ^ Month
  -> b
  -- ^ Day
  -> Maybe MonthDay
c'MonthDay'Ints sep m d
  | m == 1 = (Jan D0z'0 One sep) <$> c'Days31'Int d
  | m == 2 = (Feb D0z'0 Two sep) <$> c'Days28'Int d
  | m == 3 = (Mar D0z'0 Three sep) <$> c'Days31'Int d
  | m == 4 = (Apr D0z'0 Four sep) <$> c'Days30'Int d
  | m == 5 = (May D0z'0 Five sep) <$> c'Days31'Int d
  | m == 6 = (Jun D0z'0 Six sep) <$> c'Days30'Int d
  | m == 7 = (Jul D0z'0 Seven sep) <$> c'Days31'Int d
  | m == 8 = (Aug D0z'0 Eight sep) <$> c'Days31'Int d
  | m == 9 = (Sep D0z'0 Nine sep) <$> c'Days30'Int d
  | m == 10 = (Oct One D0z'0 sep) <$> c'Days31'Int d
  | m == 11 = (Nov One One sep) <$> c'Days30'Int d
  | m == 12 = (Dec One Two sep) <$> c'Days31'Int d
  | otherwise = Nothing

intsFromMonthDay
  :: (Integral a, Integral b)
  => MonthDay
  -> (a, b)
intsFromMonthDay md = case md of
  Jan _ _ _ d -> (1, c'Int'Days31 d)
  Feb _ _ _ d -> (2, c'Int'Days28 d)
  Mar _ _ _ d -> (3, c'Int'Days31 d)
  Apr _ _ _ d -> (4, c'Int'Days30 d)
  May _ _ _ d -> (5, c'Int'Days31 d)
  Jun _ _ _ d -> (6, c'Int'Days30 d)
  Jul _ _ _ d -> (7, c'Int'Days31 d)
  Aug _ _ _ d -> (8, c'Int'Days31 d)
  Sep _ _ _ d -> (9, c'Int'Days30 d)
  Oct _ _ _ d -> (10, c'Int'Days31 d)
  Nov _ _ _ d -> (11, c'Int'Days30 d)
  Dec _ _ _ d -> (12, c'Int'Days31 d)

pMonthDay :: Parser MonthDay
pMonthDay =
  Jan <$> pD0z <*> pOne <*> pDateSep <*> pDays31
  <|> Feb <$> pD0z <*> pTwo <*> pDateSep <*> pDays28
  <|> Mar <$> pD0z <*> pThree <*> pDateSep <*> pDays31
  <|> Apr <$> pD0z <*> pFour <*> pDateSep <*> pDays30
  <|> May <$> pD0z <*> pFive <*> pDateSep <*> pDays31
  <|> Jun <$> pD0z <*> pSix <*> pDateSep <*> pDays30
  <|> Jul <$> pD0z <*> pSeven <*> pDateSep <*> pDays31
  <|> Aug <$> pD0z <*> pEight <*> pDateSep <*> pDays31
  <|> Sep <$> pD0z <*> pNine <*> pDateSep <*> pDays30
  <|> Oct <$> pOne <*> pD0z <*> pDateSep <*> pDays31
  <|> Nov <$> pOne <*> pOne <*> pDateSep <*> pDays30
  <|> Dec <$> pOne <*> pTwo <*> pDateSep <*> pDays31

rMonthDay :: MonthDay -> ShowS
rMonthDay md = case md of
  Jan o0 o1 s d -> rD0z o0 . rOne o1 . rDateSep s . rDays31 d
  Feb o0 o1 s d -> rD0z o0 . rTwo o1 . rDateSep s . rDays28 d
  Mar o0 o1 s d -> rD0z o0 . rThree o1 . rDateSep s . rDays31 d
  Apr o0 o1 s d -> rD0z o0 . rFour o1 . rDateSep s . rDays30 d
  May o0 o1 s d -> rD0z o0 . rFive o1 . rDateSep s . rDays31 d
  Jun o0 o1 s d -> rD0z o0 . rSix o1 . rDateSep s . rDays30 d
  Jul o0 o1 s d -> rD0z o0 . rSeven o1 . rDateSep s . rDays31 d
  Aug o0 o1 s d -> rD0z o0 . rEight o1 . rDateSep s . rDays31 d
  Sep o0 o1 s d -> rD0z o0 . rNine o1 . rDateSep s . rDays30 d
  Oct o0 o1 s d -> rOne o0 . rD0z o1 . rDateSep s . rDays31 d
  Nov o0 o1 s d -> rOne o0 . rOne o1 . rDateSep s . rDays30 d
  Dec o0 o1 s d -> rOne o0 . rTwo o1 . rDateSep s . rDays31 d


instance Parseable MonthDay where parser = pMonthDay
instance Renderable MonthDay where render = rMonthDay

data Year = Year D9z D9z D9z D9z
  deriving (Eq, Ord, Show)

pYear :: Parser Year
pYear = Year <$> pD9z <*> pD9z <*> pD9z <*> pD9z

rYear :: Year -> ShowS
rYear (Year d0 d1 d2 d3) = rD9z d0 . rD9z d1 . rD9z d2 . rD9z d3

instance Parseable Year where parser = pYear
instance Renderable Year where render = rYear

c'Int'Year :: Integral a => Year -> a
c'Int'Year (Year d3 d2 d1 d0)
  = places 3 d3 + places 2 d2 + places 1 d1 + places 0 d0
  where
    places p d = digitToInt d * 10 ^ (p :: Int)

c'Year'Int :: Integral a => a -> Maybe Year
c'Year'Int a
  | a < 0 = Nothing
  | a > 9999 = Nothing
  | otherwise = do
      let (thou, remThou) = a `divMod` 1000
      d0 <- intToDigit thou
      let (hun, remHun) = remThou `divMod` 100
      d1 <- intToDigit hun
      let (ten, remTen) = remHun `divMod` 10
      d2 <- intToDigit ten
      d3 <- intToDigit remTen
      return $ Year d0 d1 d2 d3

data NonLeapDay = NonLeapDay Year DateSep MonthDay
  deriving (Eq, Ord, Show)

pNonLeapDay :: Parser NonLeapDay
pNonLeapDay = NonLeapDay <$> pYear <*> pDateSep <*> pMonthDay

rNonLeapDay :: NonLeapDay -> ShowS
rNonLeapDay (NonLeapDay y s m) = rYear y . rDateSep s . rMonthDay m

instance Parseable NonLeapDay where parser = pNonLeapDay
instance Renderable NonLeapDay where render = rNonLeapDay

data Mod4
  = L04 Zero Four
  | L08 Zero Eight
  | L12 One Two
  | L16 One Six
  | L20 Two Zero
  | L24 Two Four
  | L28 Two Eight
  | L32 Three Two
  | L36 Three Six
  | L40 Four Zero
  | L44 Four Four
  | L48 Four Eight
  | L52 Five Two
  | L56 Five Six
  | L60 Six Zero
  | L64 Six Four
  | L68 Six Eight
  | L72 Seven Two
  | L76 Seven Six
  | L80 Eight Zero
  | L84 Eight Four
  | L88 Eight Eight
  | L92 Nine Two
  | L96 Nine Six
  deriving (Eq, Ord, Show)

pMod4 :: Parser Mod4
pMod4
  = L04 <$> pZero <*> pFour
  <|> L08 <$> pZero <*> pEight
  <|> L12 <$> pOne <*> pTwo
  <|> L16 <$> pOne <*> pSix
  <|> L20 <$> pTwo <*> pZero
  <|> L24 <$> pTwo <*> pFour
  <|> L28 <$> pTwo <*> pEight
  <|> L32 <$> pThree <*> pTwo
  <|> L36 <$> pThree <*> pSix
  <|> L40 <$> pFour <*> pZero
  <|> L44 <$> pFour <*> pFour
  <|> L48 <$> pFour <*> pEight
  <|> L52 <$> pFive <*> pTwo
  <|> L56 <$> pFive <*> pSix
  <|> L60 <$> pSix <*> pZero
  <|> L64 <$> pSix <*> pFour
  <|> L68 <$> pSix <*> pEight
  <|> L72 <$> pSeven <*> pTwo
  <|> L76 <$> pSeven <*> pSix
  <|> L80 <$> pEight <*> pZero
  <|> L84 <$> pEight <*> pFour
  <|> L88 <$> pEight <*> pEight
  <|> L92 <$> pNine <*> pTwo
  <|> L96 <$> pNine <*> pSix

rMod4 :: Mod4 -> ShowS
rMod4 m4 = (++) $ case m4 of
  { L04 _ _ -> "04"; L08 _ _ -> "08"; L12 _ _ -> "12"; L16 _ _ -> "16";
    L20 _ _ -> "20"; L24 _ _ -> "24"; L28 _ _ -> "28"; L32 _ _ -> "32";
    L36 _ _ -> "36"; L40 _ _ -> "40"; L44 _ _ -> "44"; L48 _ _ -> "48";
    L52 _ _ -> "52"; L56 _ _ -> "56"; L60 _ _ -> "60"; L64 _ _ -> "64";
    L68 _ _ -> "68"; L72 _ _ -> "72"; L76 _ _ -> "76"; L80 _ _ -> "80";
    L84 _ _ -> "84"; L88 _ _ -> "88"; L92 _ _ -> "92"; L96 _ _ -> "96" }

instance Parseable Mod4 where parser = pMod4
instance Renderable Mod4 where render = rMod4

c'Int'Mod4 :: Integral a => Mod4 -> a
c'Int'Mod4 m4 = case m4 of
  { L04 _ _ -> 04; L08 _ _ -> 08; L12 _ _ -> 12; L16 _ _ -> 16;
    L20 _ _ -> 20; L24 _ _ -> 24; L28 _ _ -> 28; L32 _ _ -> 32;
    L36 _ _ -> 36; L40 _ _ -> 40; L44 _ _ -> 44; L48 _ _ -> 48;
    L52 _ _ -> 52; L56 _ _ -> 56; L60 _ _ -> 60; L64 _ _ -> 64;
    L68 _ _ -> 68; L72 _ _ -> 72; L76 _ _ -> 76; L80 _ _ -> 80;
    L84 _ _ -> 84; L88 _ _ -> 88; L92 _ _ -> 92; L96 _ _ -> 96 }

c'Mod4'Int :: Integral a => a -> Maybe Mod4
c'Mod4'Int a = case a of
  4 -> Just $ L04 Zero Four
  8 -> Just $ L08 Zero Eight
  12 -> Just $ L12 One Two
  16 -> Just $ L16 One Six
  20 -> Just $ L20 Two Zero
  24 -> Just $ L24 Two Four
  28 -> Just $ L28 Two Eight
  32 -> Just $ L32 Three Two
  36 -> Just $ L36 Three Six
  40 -> Just $ L40 Four Zero
  44 -> Just $ L44 Four Four
  48 -> Just $ L48 Four Eight
  52 -> Just $ L52 Five Two
  56 -> Just $ L56 Five Six
  60 -> Just $ L60 Six Zero
  64 -> Just $ L64 Six Four
  68 -> Just $ L68 Six Eight
  72 -> Just $ L72 Seven Two
  76 -> Just $ L76 Seven Six
  80 -> Just $ L80 Eight Zero
  84 -> Just $ L84 Eight Four
  88 -> Just $ L88 Eight Eight
  92 -> Just $ L92 Nine Two
  96 -> Just $ L96 Nine Six
  _ -> Nothing


data CenturyLeapYear
  = CenturyLeapYear Mod4 D0z D0z
  deriving (Eq, Ord, Show)

instance Parseable CenturyLeapYear where parser = pCenturyLeapYear
instance Renderable CenturyLeapYear where render = rCenturyLeapYear

c'CenturyLeapYear'Int :: Integral a => a -> Maybe CenturyLeapYear
c'CenturyLeapYear'Int a
  | a < 0 = Nothing
  | a > 9999 = Nothing
  | rm /= 0 = Nothing
  | otherwise = do
      m4 <- c'Mod4'Int qt
      return $ CenturyLeapYear m4 D0z'0 D0z'0
  where
    (qt, rm) = a `divMod` 100

c'Int'CenturyLeapYear :: Integral a => CenturyLeapYear -> a
c'Int'CenturyLeapYear (CenturyLeapYear m4 _ _) =
  c'Int'Mod4 m4 * 100

pCenturyLeapYear :: Parser CenturyLeapYear
pCenturyLeapYear = CenturyLeapYear <$> pMod4 <*> pD0z <*> pD0z

rCenturyLeapYear :: CenturyLeapYear -> ShowS
rCenturyLeapYear (CenturyLeapYear m4 d01 d02)
  = rMod4 m4 . rD0z d01 . rD0z d02

data NonCenturyLeapYear
  = NonCenturyLeapYear D9z D9z Mod4
  deriving (Eq, Ord, Show)

instance Parseable NonCenturyLeapYear where parser = pNonCenturyLeapYear
instance Renderable NonCenturyLeapYear where render = rNonCenturyLeapYear

c'NonCenturyLeapYear'Int :: Integral a => a -> Maybe NonCenturyLeapYear
c'NonCenturyLeapYear'Int a
  | a > 9999 = Nothing
  | a < 0 = Nothing
  | otherwise = do
      let (thou, rmThou) = a `divMod` 1000
      d0 <- intToDigit thou
      let (hun, rmHun) = rmThou `divMod` 100
      d1 <- intToDigit hun
      m4 <- c'Mod4'Int rmHun
      return $ NonCenturyLeapYear d0 d1 m4

c'Int'NonCenturyLeapYear :: Integral a => NonCenturyLeapYear -> a
c'Int'NonCenturyLeapYear (NonCenturyLeapYear d0 d1 m4)
  = places 3 d0 + places 2 d1 + c'Int'Mod4 m4
  where
    places p d = digitToInt d * 10 ^ (p :: Int)

pNonCenturyLeapYear :: Parser NonCenturyLeapYear
pNonCenturyLeapYear = NonCenturyLeapYear <$> pD9z <*> pD9z <*> pMod4

rNonCenturyLeapYear :: NonCenturyLeapYear -> ShowS
rNonCenturyLeapYear (NonCenturyLeapYear d0 d1 m4)
  = rD9z d0 . rD9z d1 . rMod4 m4

data LeapDay = LeapDay
  (Either CenturyLeapYear NonCenturyLeapYear)
  DateSep
  D0z Two
  DateSep
  Two Nine
  deriving (Eq, Ord, Show)

pLeapDay :: Parser LeapDay
pLeapDay
  = LeapDay
  <$> (Left <$> pCenturyLeapYear <|> Right <$> pNonCenturyLeapYear)
  <*> pDateSep
  <*> pD0z
  <*> pTwo
  <*> pDateSep
  <*> pTwo
  <*> pNine

rLeapDay :: LeapDay -> ShowS
rLeapDay (LeapDay y s1 m0 m2 s2 d2 d9)
  = either rCenturyLeapYear rNonCenturyLeapYear y
  . rDateSep s1
  . rD0z m0
  . rTwo m2
  . rDateSep s2
  . rTwo d2
  . rNine d9

instance Parseable LeapDay where parser = pLeapDay
instance Renderable LeapDay where render = rLeapDay

newtype DateA = DateA (Either NonLeapDay LeapDay)
  deriving (Eq, Ord, Show)

pDateA :: Parser DateA
pDateA = DateA <$> (Left <$> pNonLeapDay <|> Right <$> pLeapDay)

rDateA :: DateA -> ShowS
rDateA (DateA ei) = either rNonLeapDay rLeapDay ei

instance Parseable DateA where parser = pDateA
instance Renderable DateA where render = rDateA

c'Day'DateA :: DateA -> Day
c'Day'DateA (DateA ei) = maybe (error "c'Day'DateA: error") id
  $ fromGregorianValid yi mi di
  where
    (yi, mi, di) = case ei of
      Left (NonLeapDay y _ md) -> (c'Int'Year y, m, d)
        where
          (m, d) = intsFromMonthDay md
      Right (LeapDay eiYear _ _ _ _ _ _) ->
        ( either c'Int'CenturyLeapYear c'Int'NonCenturyLeapYear eiYear
        , 2, 29)

c'DateA'Day :: DateSep -> Day -> Maybe DateA
c'DateA'Day sep dy
  | d == 29 = fmap (DateA . Right) leapDay
  | otherwise = fmap (DateA . Left) nonLeapDay
  where
    (y, m, d) = toGregorian dy
    leapDay = do
      yr <-     fmap Left (c'CenturyLeapYear'Int y)
            <|> fmap Right (c'NonCenturyLeapYear'Int y)
      return $ LeapDay yr sep D0z'0 Two sep Two Nine
    nonLeapDay = do
      yr <- c'Year'Int y
      md <- c'MonthDay'Ints sep m d
      return $ NonLeapDay yr sep md
