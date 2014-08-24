{-# LANGUAGE FlexibleInstances #-}
module Penny.Copper.Render where

import Penny.Numbers.Natural hiding (length)
import Penny.Numbers.Abstract.Aggregates
import Data.Maybe (fromMaybe)
import Data.List (genericLength)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as X
import Text.Parsec.Text (Parser)
import Deka.Native.Abstract
import Text.Parsec.Pos
import Text.Parsec
import Data.Sequence (Seq, fromList)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.Unpolar
import Data.Monoid
import Penny.Numbers.Abstract.RadGroup
import Data.Sums

-- | Things that can be rendered.  Properties of renderable items:
--
-- * 'render' is an injective function:
-- <http://en.wikipedia.org/wiki/Injective_function>
--
-- * 'render' never produces a null 'Text'
--
-- * The function
--
-- @
--   let run x = 'parser' x 'Control.Applicative.<*' 'Text.Parsec.Combinator.eof' in
--   'Data.Either.Combinators.fromRight' '.' 'Text.Parsec.Prim.parse' 'run' \"\"
-- @
--
-- is the left inverse of 'render'.
--
-- * 'parser' does not accept an empty string; that is, if 'parser'
-- succeeds, it consumes at least one character.

class Renderable a where
  render :: a -> Text
  parser :: Parser a

instance Renderable Novem where
  render = X.singleton . novemToChar
  parser = tokenPrim (:[]) (\ps t _ -> updatePosChar ps t)
    charToNovem

instance Renderable Decem where
  render = X.singleton . decemToChar
  parser = tokenPrim (:[]) (\ps t _ -> updatePosChar ps t)
    charToDecem

-- | Renders a sequence of renderable things.  Does not necessarily
-- obey the 'Renderable' laws, because the sequence might be empty.

renderSequence'
  :: (Renderable a, F.Foldable f, Functor f)
  => f a
  -> Text
renderSequence' = X.concat . F.toList . fmap render

-- | Parses a sequence of renderable things.

parseSequence' :: Renderable a => Parser (Seq a)
parseSequence' = fmap fromList $ many parser

renderSequence
  :: (F.Foldable f, Functor f)
  => (a -> Text)
  -> f a
  -> Text
renderSequence f = X.concat . F.toList . fmap f

parseSequence
  :: Parser a
  -> Parser (Seq a)
parseSequence p = fmap fromList $ many p

instance Renderable NovDecs where
  render (NovDecs n ds) = render n <> renderSequence' ds
  parser = liftM2 NovDecs parser parseSequence'

instance Renderable ZeroesNovDecs where
  render (ZeroesNovDecs z nd) = renderNonNegZeroes z
    <> render nd
  parser = liftM2 ZeroesNovDecs parseNonNegZeroes parser

instance Renderable DecDecs where
  render (DecDecs d ds) = render d <> renderSequence' ds
  parser = liftM2 DecDecs parser parseSequence'


instance Renderable Zeroes where
  render (Zeroes p)
    | i > fromIntegral (maxBound :: Int) =
        error "zeroes: too many zeroes!"
    | otherwise = X.replicate (fromIntegral i) (X.singleton '0')
    where
      i = unPos p

  parser = fmap f $ many1 (char '0')
    where
      f = Zeroes . fromMaybe (error "zeroes: parser: error")
        . pos . genericLength


-- | Render zero or more zeroes.  Does not respect the Renderable laws
-- as the resulting list might be empty.
renderNonNegZeroes :: NonNeg -> Text
renderNonNegZeroes nn
  | i > fromIntegral (maxBound :: Int) =
      error "renderZeroes: too many zeroes!"
  | otherwise = X.replicate (fromIntegral i) (X.singleton '0')
  where
    i = unNonNeg nn

parseNonNegZeroes :: Parser NonNeg
parseNonNegZeroes = do
  zs <- many1 (char '0')
  maybe (error "parseNonNegZeroes: error") return
    . nonNeg . fromIntegral . length $ zs

instance Renderable UNWhole where
  render = render . unUNWhole
  parser = fmap UNWhole parser

instance Renderable Period where
  render _ = X.singleton ','
  parser = char ',' >> return Comma

instance Renderable Comma where
  render _ = X.singleton '.'
  parser = char '.' >> return Period

instance Renderable a => Renderable (Grouper a) where
  render a = case a of
    Space -> X.singleton ' '
    Thin -> X.singleton '\x2009'
    Under -> X.singleton '_'
    Unique b -> render b

  parser =
    (char ' ' >> return Space)
    <|> (char '\x2009' >> return Thin)
    <|> (char '_' >> return Under)
    <|> fmap Unique parser

rPeriod :: Radix Period -> Text
rPeriod _ = X.singleton '.'

pPeriod :: Parser (Radix Period)
pPeriod = char '.' >> return radPeriod

rComma :: Radix Comma -> Text
rComma _ = X.singleton ','

pComma :: Parser (Radix Comma)
pComma = char ',' >> return radComma

instance Renderable (Radix Period) where
  render = rPeriod
  parser = pPeriod

instance Renderable (Radix Comma) where
  render = rComma
  parser = pComma

rGroup :: (Grouper g -> Text) -> (p -> Text) -> Group g p -> Text
rGroup rg rp (Group g p) = rg g <> rp p

pGroup :: Parser (Grouper g) -> Parser p -> Parser (Group g p)
pGroup pg pp = liftM2 Group pg pp

instance (Renderable r, Renderable b) => Renderable (Group r b) where
  render = rGroup render render
  parser = pGroup parser parser

rUNWholeRadix :: (Radix r -> Text) -> UNWholeRadix r -> Text
rUNWholeRadix rr (UNWholeRadix nd rdx md) =
  render nd <> rr rdx <> maybe X.empty render md

pUNWholeRadix :: Parser (Radix r) -> Parser (UNWholeRadix r)
pUNWholeRadix pr = liftM3 UNWholeRadix parser pr (optionMaybe parser)

instance Renderable (UNWholeRadix Period) where
  render = rUNWholeRadix render
  parser = pUNWholeRadix parser

instance Renderable (UNWholeRadix Comma) where
  render = rUNWholeRadix render
  parser = pUNWholeRadix parser

-- | HasZeroDigit does not follow 'Renderable' properties, as its
-- parser succeeds on the empty input, and its renderer may produce no
-- input

renderZeroDigit :: HasZeroDigit -> Text
renderZeroDigit (HasZeroDigit zd) | zd = X.singleton '0'
                                  | otherwise = X.empty

parseZeroDigit :: Parser HasZeroDigit
parseZeroDigit = fmap HasZeroDigit $ (char '0' >> return True)
                                     <|> return False

rUNRadFrac :: (Radix r -> Text) -> UNRadFrac r -> Text
rUNRadFrac rr (UNRadFrac z rd znd) = renderZeroDigit z
  <> rr rd <> render znd

pUNRadFrac :: Parser (Radix r) -> Parser (UNRadFrac r)
pUNRadFrac pr = liftM3 UNRadFrac parseZeroDigit pr parser

instance Renderable (UNRadFrac Period) where
  render = rUNRadFrac render
  parser = pUNRadFrac parser

instance Renderable (UNRadFrac Comma) where
  render = rUNRadFrac render
  parser = pUNRadFrac parser

instance Renderable UZZeroOnly where
  render _ = X.singleton '0'
  parser = char '0' >> return UZZeroOnly

rUZTrailing :: (Radix r -> Text) -> UZTrailing r -> Text
rUZTrailing rr (UZTrailing z r mz) =
  renderZeroDigit z <> rr r <> maybe X.empty render mz

pUZTrailing :: Parser (Radix r) -> Parser (UZTrailing r)
pUZTrailing pr = liftM3 UZTrailing parseZeroDigit pr (optionMaybe parser)

instance Renderable (UZTrailing Period) where
  render = rUZTrailing render
  parser = pUZTrailing parser

instance Renderable (UZTrailing Comma) where
  render = rUZTrailing render
  parser = pUZTrailing parser


rGZ
  :: (Radix r -> Text)
  -> (Group r Zeroes -> Text)
  -> GZ r
  -> Text
rGZ rr rg (GZ hz r z g1 gs) = renderZeroDigit hz <> rr r
    <> render z <> rg g1 <> renderSequence rg gs

pGZ
  :: Parser (Radix r)
  -> Parser (Group r Zeroes)
  -> Parser (GZ r)
pGZ pr pg = liftM5 GZ parseZeroDigit pr parser pg (parseSequence pg)

instance Renderable (GZ Period) where
  render = rGZ render render
  parser = pGZ parser parser

instance Renderable (GZ Comma) where
  render = rGZ render render
  parser = pGZ parser parser


rMasunoGroupedLeft
  :: (Group r DecDecs -> Text)
  -> MasunoGroupedLeft r
  -> Text
rMasunoGroupedLeft rg (MasunoGroupedLeft nd g1 gs) =
  render nd <> rg g1 <> renderSequence rg gs

pMasunoGroupedLeft
  :: Parser (Group r DecDecs)
  -> Parser (MasunoGroupedLeft r)
pMasunoGroupedLeft pg = liftM3 MasunoGroupedLeft parser pg
  (parseSequence pg)


instance Renderable (MasunoGroupedLeft Period) where
  render = rMasunoGroupedLeft render
  parser = pMasunoGroupedLeft parser

instance Renderable (MasunoGroupedLeft Comma) where
  render = rMasunoGroupedLeft render
  parser = pMasunoGroupedLeft parser

rMasunoGroupedLeftRad
  :: (Radix r -> Text)
  -> (Group r DecDecs -> Text)
  -> MasunoGroupedLeftRad r
  -> Text
rMasunoGroupedLeftRad rr rg (MasunoGroupedLeftRad mgl r my) =
    rMasunoGroupedLeft rg mgl <> rr r
    <> maybe X.empty (\(dd, sq) -> render dd <> renderSequence rg sq) my

pMasunoGroupedLeftRad
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (MasunoGroupedLeftRad r)
pMasunoGroupedLeftRad pr pg = liftM3 MasunoGroupedLeftRad
  (pMasunoGroupedLeft pg)
  pr (optionMaybe (liftM2 (,) parser (parseSequence pg)))

instance Renderable (MasunoGroupedLeftRad Period) where
  render = rMasunoGroupedLeftRad render render
  parser = pMasunoGroupedLeftRad parser parser

instance Renderable (MasunoGroupedLeftRad Comma) where
  render = rMasunoGroupedLeftRad render render
  parser = pMasunoGroupedLeftRad parser parser

rMasunoGroupedRight
  :: (Radix r -> Text)
  -> (Group r DecDecs -> Text)
  -> MasunoGroupedRight r
  -> Text
rMasunoGroupedRight rr rg (MasunoGroupedRight nd rd dd g1 gs) =
  render nd <> rr rd <> render dd <> rg g1
    <> renderSequence rg gs

pMasunoGroupedRight
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (MasunoGroupedRight r)
pMasunoGroupedRight pr pg = liftM5 MasunoGroupedRight parser
  pr parser pg (parseSequence pg)

instance Renderable (MasunoGroupedRight Comma) where
  render = rMasunoGroupedRight render render
  parser = pMasunoGroupedRight parser parser

instance Renderable (MasunoGroupedRight Period) where
  render = rMasunoGroupedRight render render
  parser = pMasunoGroupedRight parser parser

rFracunoFirstGroupZ
  :: (Radix r -> Text)
  -> (Group r Zeroes -> Text)
  -> (Group r ZeroesNovDecs -> Text)
  -> (Group r DecDecs -> Text)
  -> FracunoFirstGroupZ r
  -> Text
rFracunoFirstGroupZ rr rz rznd rd (FracunoFirstGroupZ hz rdx z gz g gs) =
  renderZeroDigit hz <> rr rdx <> render z <> renderSequence rz gz
    <> rznd g <> renderSequence rd gs

pFracunoFirstGroupZ
  :: Parser (Radix r)
  -> Parser (Group r Zeroes)
  -> Parser (Group r ZeroesNovDecs)
  -> Parser (Group r DecDecs)
  -> Parser (FracunoFirstGroupZ r)
pFracunoFirstGroupZ pr pz pznd pd =
  FracunoFirstGroupZ
  `fmap` parseZeroDigit
  `ap` pr `ap` parser `ap` parseSequence pz
  `ap` pznd `ap` parseSequence pd

instance Renderable (FracunoFirstGroupZ Period) where
  render = rFracunoFirstGroupZ render render render render
  parser = pFracunoFirstGroupZ parser parser parser parser

instance Renderable (FracunoFirstGroupZ Comma) where
  render = rFracunoFirstGroupZ render render render render
  parser = pFracunoFirstGroupZ parser parser parser parser

rFracunoFirstGroupNZ
  :: (Radix r -> Text)
  -> (Group r DecDecs -> Text)
  -> FracunoFirstGroupNZ r
  -> Text
rFracunoFirstGroupNZ rr rd (FracunoFirstGroupNZ hz r znd g1 gs) =
  renderZeroDigit hz <> rr r <> render znd <> rd g1
    <> renderSequence rd gs

pFracunoFirstGroupNZ
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (FracunoFirstGroupNZ r)
pFracunoFirstGroupNZ pr pg =
  liftM5 FracunoFirstGroupNZ parseZeroDigit pr parser
      pg (parseSequence pg)

instance Renderable (FracunoFirstGroupNZ Period) where
  render = rFracunoFirstGroupNZ render render
  parser = pFracunoFirstGroupNZ parser parser

instance Renderable (FracunoFirstGroupNZ Comma) where
  render = rFracunoFirstGroupNZ render render
  parser = pFracunoFirstGroupNZ parser parser


-- Aggregates

rUngroupedZero
  :: (Radix r -> Text)
  -> UngroupedZero r
  -> Text
rUngroupedZero rr = caseS2 render (rUZTrailing rr)
  . unUngroupedZero

-- If there is NOT a zero digit, this must be a UZTrailing.  If
-- there is a zero digit, see if we have a radix; if so, this is a
-- UZTrailing; otherwise, it's a UZZeroOnly.
pUngroupedZero
  :: Parser (Radix r)
  -> Parser (UngroupedZero r)
pUngroupedZero pr = parseZeroDigit >>= f
  where
    f (HasZeroDigit z)
      | z = do
          mayRdx <- optionMaybe pr
          fmap UngroupedZero $ case mayRdx of
            Nothing -> return $ S2a UZZeroOnly
            Just rdx -> do
              zs <- optionMaybe parser
              return . S2b $
                UZTrailing (HasZeroDigit True) rdx zs

      | otherwise = do
          rdx <- pr
          zs <- optionMaybe parser
          return . UngroupedZero . S2b
            $ UZTrailing (HasZeroDigit False) rdx zs


instance Renderable (UngroupedZero Comma) where
  render = rUngroupedZero render
  parser = pUngroupedZero parser

instance Renderable (UngroupedZero Period) where
  render = rUngroupedZero render
  parser = pUngroupedZero parser

-- | Parse an 'UngroupedNonZero'.  Sees if the input text leads off
-- with a 'NovDecs'.  If so, this is either a 'UNWhole' or a
-- 'UNWholeRadix', which is determined by seeing whether a 'Radix'
-- comes next.  If input text does not lead off with a 'NovDecs', this
-- must be a 'UNRadFrac'.

pUngroupedNonZero
  :: Parser (Radix r)
  -> Parser (UngroupedNonZero r)
pUngroupedNonZero pr = do
  mayNd <- optionMaybe parser
  case mayNd of
    Nothing -> fmap (UngroupedNonZero . S3c) (pUNRadFrac pr)
    Just nd -> do
      mayRdx <- optionMaybe pr
      case mayRdx of
        Nothing -> return . UngroupedNonZero . S3a . UNWhole $ nd
        Just rdx -> do
          dd <- optionMaybe parser
          return . UngroupedNonZero . S3b $ UNWholeRadix nd rdx dd

rUngroupedNonZero
  :: (Radix r -> Text)
  -> UngroupedNonZero r
  -> Text
rUngroupedNonZero rr =
  caseS3 render (rUNWholeRadix rr) (rUNRadFrac rr)
  . unUngroupedNonZero

instance Renderable (UngroupedNonZero Period) where
  render = rUngroupedNonZero render
  parser = pUngroupedNonZero parser

instance Renderable (UngroupedNonZero Comma) where
  render = rUngroupedNonZero render
  parser = pUngroupedNonZero parser

rUngroupedUnpolar
  :: (Radix r -> Text)
  -> UngroupedUnpolar r
  -> Text
rUngroupedUnpolar rr =
  caseS2 (rUngroupedZero rr) (rUngroupedNonZero rr)
  . unUngroupedUnpolar

-- | Parses an 'UngroupedUnpolar'.
--
-- If a 'NovDecs' is first, this is a
-- 'UNWhole' or 'UNWholeRadix'; which is determined by whether a radix
-- is next.
--
-- If a radix is first, this is a 'UNRadFrac' or a
-- 'UZTrailing'; which is determined by whether there are any NovDecs
-- following the radix.
--
-- If a zero is first, this is a 'UZZeroOnly', 'UZTrailing', or
-- 'UNRadFrac'; which is determined first by whether a radix follows
-- the zero (which narrows it down to 'UZTrailing' or 'UNRadFrac' if
-- there is) and then whether there are any NovDecs following the
-- radix (this is a 'UNRadFrac' if there are.)

pUngroupedUnpolar :: Parser (Radix r) -> Parser (UngroupedUnpolar r)
pUngroupedUnpolar pr = fmap UngroupedUnpolar $
  fmap (S2b . UngroupedNonZero) (firstNovDecs pr)
  <|> firstRadix pr
  <|> firstZero pr

-- | Parses either a 'UNWhole' or a 'UNWholeRadix' where the first set
-- of characters is a 'NovDecs'.
firstNovDecs
  :: Parser (Radix r)
  -> Parser (S3 UNWhole (UNWholeRadix r) a)
firstNovDecs pr = do
  nd <- parser
  mayRdx <- optionMaybe pr
  case mayRdx of
    Nothing -> return . S3a $ UNWhole nd
    Just rdx -> do
      dd <- optionMaybe parser
      return . S3b $ UNWholeRadix nd rdx dd


-- | Parses either an 'UngroupedZero' or an 'UngroupedNonZero' where
-- the first character is a radix point.
firstRadix
  :: Parser (Radix r)
  -> Parser (S2 (UngroupedZero r) (UngroupedNonZero r))
firstRadix pr = do
  rdx <- pr
  zs <- parseNonNegZeroes
  mayNd <- optionMaybe parser
  case mayNd of
    Nothing -> return . S2a . UngroupedZero
      . S2b $ UZTrailing hzd rdx mz
      where
        mz = fmap Zeroes . nonNegToPos $ zs

    Just nd -> return . S2b . UngroupedNonZero
      . S3c $ UNRadFrac hzd rdx (ZeroesNovDecs zs nd)
  where
    hzd = HasZeroDigit False

-- | Parses either an 'UngroupedZero' or an 'UngroupedNonZero' where
-- the first character is a zero.
firstZero
  :: Parser (Radix r)
  -> Parser (S2 (UngroupedZero r) (UngroupedNonZero r))
firstZero pr = do
  _ <- char '0'
  mayRdx <- optionMaybe pr
  case mayRdx of
    Nothing -> return . S2a . UngroupedZero . S2a $ UZZeroOnly
    Just rdx -> do
      mayZs <- optionMaybe parser
      mayNd <- optionMaybe parser
      case mayNd of
        Nothing -> return . S2a . UngroupedZero
          . S2b $ UZTrailing hzd rdx mayZs
        Just nd -> return . S2b
          . UngroupedNonZero . S3c $ UNRadFrac hzd
          rdx (ZeroesNovDecs nnz nd)
          where
            nnz = case mayZs of
              Nothing -> nonNegZero
              Just (Zeroes ps) -> posToNonNeg ps
  where
    hzd = HasZeroDigit True

instance Renderable (UngroupedUnpolar Comma) where
  render = rUngroupedUnpolar render
  parser = pUngroupedUnpolar parser

instance Renderable (UngroupedUnpolar Period) where
  render = rUngroupedUnpolar render
  parser = pUngroupedUnpolar parser

{-

Unpolar
  UngroupedUnpolar
    UngroupedZero
      UZZeroOnly
      UZTrailing
        HasZeroDigit * Radix * Maybe Zeroes
    UngroupedNonZero
      UNWhole
        NovDecs
      UNWholeRadix
        NovDecs * Radix * Maybe DecDecs
      UNRadFrac
        HasZeroDigit * Radix * (NonNegZeroes * NovDecs)
  GroupedUnpolar
    GZ
      HasZeroDigit * Radix * Zeroes * (Group Zeroes) * (Seq (Group Zeroes))
    GroupedNonZero
      MasunoGroupedLeft
        NovDecs * (Group DecDecs) * (Seq (Group DecDecs))
      MasunoGroupedLeftRad
        MasunoGroupedLeft * Radix * Maybe (DecDecs, Seq (Group DecDecs))
      MasunoGroupedRight
        NovDecs * Radix * DecDecs * Group DecDecs * Seq (Group DecDecs)
      FracunoFirstGroupZ
        HasZeroDigit * Radix * Zeroes * Seq (Group Zeroes)
          * Group ZeroesNovDecs * Seq (Group DecDecs)
      FracunoFirstGroupNZ
        HasZeroDigit * Radix * ZeroesNovDecs * Group DecDecs
          * Seq (Group DecDecs)
-}

{-

NovDecs First:

UNWhole
  NovDecs
UNWholeRadix
  NovDecs * Radix * Maybe DecDecs
MasunoGroupedRight
  NovDecs * Radix * DecDecs * Group DecDecs * Seq (Group DecDecs)

MasunoGroupedLeft
  NovDecs * (Group DecDecs) * (Seq (Group DecDecs))
MasunoGroupedLeftRad
  MasunoGroupedLeft * Radix * Maybe (DecDecs, Seq (Group DecDecs))

Mandatory Zero First:

UZZeroOnly

Optional Zero First:

UZTrailing
  HasZeroDigit * Radix * Maybe Zeroes
UNRadFrac
  HasZeroDigit * Radix * (NonNegZeroes * NovDecs)
FracunoFirstGroupNZ
  HasZeroDigit * Radix * (NonNegZeroes * NovDecs) * Group DecDecs
    * Seq (Group DecDecs)

GZ
  HasZeroDigit * Radix * Zeroes * (Group Zeroes) * (Seq (Group Zeroes))
FracunoFirstGroupZ
  HasZeroDigit * Radix * Zeroes * Seq (Group Zeroes)
    * Group ZeroesNovDecs * Seq (Group DecDecs)
-}

novDecsFirst
  :: Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (S5 UNWhole
                (UNWholeRadix r)
                (MasunoGroupedRight r)
                (MasunoGroupedLeft r)
                (MasunoGroupedLeftRad r))
novDecsFirst pr pg = do
  nd <- parser
  rest <- optionMaybe
    ( novDecsFirstMasunoGroupedLeft nd pr pg
      <|> novDecsFirstRadix nd pr pg )
  return $ case rest of
    Nothing -> S5a $ UNWhole nd
    Just a -> a

novDecsFirstMasunoGroupedLeft
  :: NovDecs
  -> Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (S5 a b c (MasunoGroupedLeft r) (MasunoGroupedLeftRad r))
novDecsFirstMasunoGroupedLeft nd pr pg = do
  g1 <- pg
  gs <- parseSequence pg
  let mgl = MasunoGroupedLeft nd g1 gs
  mayRdx <- optionMaybe pr
  case mayRdx of
    Nothing -> return $ S5d mgl
    Just rdx -> do
      next <- optionMaybe $ liftM2 (,) parser (parseSequence pg)
      return . S5e $ MasunoGroupedLeftRad mgl rdx next

novDecsFirstRadix
  :: NovDecs
  -> Parser (Radix r)
  -> Parser (Group r DecDecs)
  -> Parser (S5 a (UNWholeRadix r) (MasunoGroupedRight r) d e)
novDecsFirstRadix nd pr pg = do
  rdx <- pr
  mayDD <- optionMaybe parser
  case mayDD of
    Nothing -> return . S5b $ UNWholeRadix nd rdx Nothing
    Just dd -> do
      mayGr <- optionMaybe pg
      case mayGr of
        Nothing -> return . S5b $ UNWholeRadix nd rdx (Just dd)
        Just gr -> do
          sq <- parseSequence pg
          return . S5c $ MasunoGroupedRight nd rdx dd gr sq

--
--
--

optionalZeroFirst
  :: Parser (Radix r)
  -> Parser (Group r Zeroes)
  -> Parser (Group r ZeroesNovDecs)
  -> Parser (Group r DecDecs)
  -> Parser (S6 UZZeroOnly
                (UZTrailing r)
                (UNRadFrac r)
                (GZ r)
                (FracunoFirstGroupZ r)
                (FracunoFirstGroupNZ r))
optionalZeroFirst pr pz pznd pd = undefined

-- | Parses items that lead with an optional zero, followed by a
-- mandatory radix.  The optional zero must already have been parsed.
--
-- First, see if there are zeroes after the radix.  If there are NO
-- zeroes, this is a 'UZTrailing', 'UNRadFrac', or
-- 'FracunoFirstGroupNZ'.  To determine which, see if any 'NovDecs'
-- follow the zeroes.  If no 'NovDecs' follow, this is a 'UZTrailing'.
-- Otherwise, if 'NovDecs' follow, see if a group of 'DecDecs'
-- follows.  If so, this is a 'FracunoFirstGroupNZ'; otherwise, it is
-- a 'UNRadFrac'.
--
-- If there ARE zeroes after the radix, nothing has been narrowed
-- down.  See if there are 'NovDecs' next.  If so, this is a
-- 'UNRadFrac' or a 'FracunoFirstGroupNZ'; which can be determined by
-- seeing if 'DecDecs' follow.  If instead a group of zeroes follows,
-- parse any remaining groups of zeroes.  If a group of
-- 'ZeroesNovDecs' follows, this is a 'FracunoFirstGroupZ'; otherwise,
-- it is a 'GZ'.
--
-- If neither a 'NovDecs' nor a group of zeroes follows the zeroes
-- after the radix, this is a 'UZTrailing'.
zeroThenRadix
  :: HasZeroDigit
  -> Parser (Radix r)
  -> Parser (Group r Zeroes)
  -> Parser (Group r ZeroesNovDecs)
  -> Parser (Group r DecDecs)
  -> Parser (S6 a
                (UZTrailing r)
                (UNRadFrac r)
                (GZ r)
                (FracunoFirstGroupZ r)
                (FracunoFirstGroupNZ r))
zeroThenRadix hzd pr pg pznd pd = do
  rdx <- pr
  mayZs <- optionMaybe parser
  case mayZs of
    Nothing -> do
      mayNd <- optionMaybe parser
      case mayNd of
        Nothing -> return . S6b $ UZTrailing hzd rdx Nothing
        Just nd -> do
          mayDD <- optionMaybe pd
          case mayDD of
            Nothing -> return . S6c $ UNRadFrac hzd rdx
              (ZeroesNovDecs zeroNonNeg nd)
            Just dd -> do
              seqDecDecs <- parseSequence pd
              return . S6f $ FracunoFirstGroupNZ
                hzd rdx (ZeroesNovDecs zeroNonNeg nd) dd seqDecDecs

-- Start here
    Just zs -> do
      mayNd <- optionMaybe parser
      case mayNd of
        Just nd -> do
          g1 <- pd
          gs <- parseSequence pd
          let znd = ZeroesNovDecs (posToNonNeg . unZeroes $ zs) nd
          return . S6f $ FracunoFirstGroupNZ hzd rdx znd g1 gs

        Nothing -> do
          mayGzs <- optionMaybe pg
          case mayGzs of
            Nothing -> do
              znd <- pznd
              dds <- parseSequence pd
              return . S6e $ FracunoFirstGroupZ hzd rdx zs S.empty
                znd dds

            Just gzs -> do
              zeroesGroups <- parseSequence pg
              mayZnd <- optionMaybe pznd
              case mayZnd of
                Nothing -> return . S6d $ GZ hzd rdx zs gzs zeroesGroups
                Just znd -> undefined
