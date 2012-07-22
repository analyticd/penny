-- | Takes postings and places them into a tree for further
-- processing.
--
-- Steps:
--
-- * 1. [LT.PostingInfo] -> [PriceConverted]
--
-- * 2. [PriceConverted] -> FlatMap
--
-- * 3. FlatMap -> RawBals
--
-- * 4. RawBals -> (SummedBals, TotalBal)
--
-- * 5. (SummedBals, TotalBal) -> (SummedWithIsEven, TotalBal)
--
-- * 6. (SummedWithIsEven, TotalBal) -> (PreSpecMap, TotalBal)
--
-- * 7. (PreSpecMap, TotalBal) -> [Columns PreSpec]
--
-- * 8. [Columns PreSpec] -> [Columns R.ColumnSpec] (strict)
--
-- * 9. [Columns R.ColumnSpec] -> [[Chunk.Bit]] (lazy)
module Penny.Cabin.Balance.Tree (report) where

import Control.Applicative(Applicative(pure, (<*>)), (<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Control.Monad.Trans.State as St
import qualified Penny.Cabin.Row as R
import qualified Data.Foldable as Fdbl
import qualified Data.Functor.Identity as Id
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Penny.Lincoln.NestedMap as NM
import qualified Data.Text as X
import qualified Data.Traversable as Tr
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Balance as Bal
import qualified Data.Semigroup as S

-- Step 1. Convert prices.
data PriceConverted = PriceConverted {
  pcEntry :: L.Entry
  , pcAccount :: L.Account }

convertOne ::
  L.PriceDb
  -> (L.Commodity, L.DateTime)
  -> L.Entry
  -> Ex.Exceptional X.Text L.Entry
convertOne db (cty, dt) en@(L.Entry dc am@(L.Amount _ fr)) =
  if fr == cty
  then return en
  else do
    let to = L.To cty
    am' <- case L.convert db dt to am of
      Ex.Exception e -> Ex.throw (convertError cty am e)
      Ex.Success g -> return g
    return (L.Entry dc am')

convertError ::
  L.Commodity
  -> L.Amount
  -> L.PriceDbError
  -> X.Text
convertError to (L.Amount _ fr) e =
  let fromErr = L.text (L.Delimited (X.singleton ':')
                        (Fdbl.toList . L.unCommodity $ fr))
      toErr = L.text (L.Delimited (X.singleton ':')
                      (Fdbl.toList . L.unCommodity $ to))
  in case e of
    L.FromNotFound ->
      X.pack "no data to convert from commodity "
      `X.append` fromErr
    L.ToNotFound ->
      X.pack "no data to convert to commodity "
      `X.append` toErr
    L.CpuNotFound ->
      X.pack "no data to convert from commodity "
      `X.append` fromErr
      `X.append` (X.pack " to commodity ")
      `X.append` toErr
      `X.append` (X.pack " at given date and time")
  

buildDb :: [L.PricePoint] -> L.PriceDb
buildDb = foldl f L.emptyDb where
  f db pb = L.addPrice db pb

converter ::
  O.Options
  -> [L.PricePoint]
  -> [L.Box Ly.LibertyMeta]
  -> Ex.Exceptional X.Text [PriceConverted]
converter os pbs bs = case O.convert os of
  Nothing ->
    let f b = PriceConverted en ac where
          p = L.boxPostFam b
          en = Q.entry p
          ac = Q.account p
    in return . map f $ bs
  Just pair ->
    let f b = let p = L.boxPostFam b
                  en = Q.entry p
                  ac = Q.account p
                 in do
                   en' <- convertOne db pair en
                   return (PriceConverted en' ac)
        db = buildDb pbs
    in mapM f bs


-- Step 2. This puts all the PriceConverteds into a flat map, where
-- the key is the full account name and the value is the balance. If
-- the user desired, zero balances are eliminated from the flat map.
newtype FlatMap = FlatMap { _unFlatMap :: M.Map L.Account Bal.Balance }

toFlatMap :: O.Options -> [PriceConverted] -> FlatMap
toFlatMap o = FlatMap . foldr f M.empty where
  remove = not . CO.unShowZeroBalances . O.showZeroBalances $ o
  f pc m =
    let a = pcAccount pc
        bal = Bal.entryToBalance . pcEntry $ pc
    in case M.lookup a m of
      Nothing -> M.insert a bal m
      Just oldBal ->
        let added = Bal.addBalances oldBal bal
            newBal = if remove
                     then Bal.removeZeroCommodities added
                     else Just added
        in case newBal of
          Nothing -> M.delete a m
          Just b' -> M.insert a b' m

-- Step 3
newtype RawBal = RawBal { unRawBal :: S.Option Bal.Balance }
instance Monoid.Monoid RawBal where
  mappend (RawBal b1) (RawBal b2) = RawBal $ b1 `Monoid.mappend` b2
  mempty = RawBal Monoid.mempty

type RawBals = NM.NestedMap L.SubAccountName RawBal

-- | Inserts a pair from the FlatMap into the Balances NestedMap.
insertBalance ::
  L.Account
  -> Bal.Balance
  -> RawBals
  -> RawBals
insertBalance a b rbs = let
  rb = RawBal . S.Option . Just $ b
  subs = Fdbl.toList . L.unAccount $ a
  in NM.insert rbs subs rb
  
rawBalances :: FlatMap -> RawBals
rawBalances (FlatMap m) = M.foldrWithKey insertBalance NM.empty m

-- Step 4
newtype SummedBal =
  SummedBal { unSummedBal :: S.Option Bal.Balance }
instance Monoid.Monoid SummedBal where
  mappend (SummedBal b1) (SummedBal b2) =
    SummedBal $ b1 `Monoid.mappend` b2
  mempty = SummedBal Monoid.mempty
type TotalBal = SummedBal
type SummedBals = NM.NestedMap L.SubAccountName SummedBal

sumBalances :: RawBals -> (SummedBals, TotalBal)
sumBalances rb = (sb, (SummedBal . unRawBal $ tb)) where
  (tb, rawBals) = NM.cumulativeTotal rb
  sb = fmap (SummedBal . unRawBal) rawBals

-- Step 5
type SummedWithIsEven = NM.NestedMap L.SubAccountName (SummedBal, Bool)

makeSummedWithIsEven ::
  (SummedBals, TotalBal)
  -> (SummedWithIsEven, TotalBal)
makeSummedWithIsEven (sb, tb) = (swie, tb) where
  swie = St.evalState (Tr.mapM f sb) False
  f lbl = do
    st <- St.get
    St.put (not st)
    return (lbl, st)

-- Step 6
data Columns a = Columns {
  account :: a
  , drCr :: a
  , commodity :: a
  , quantity :: a
  } deriving Show

instance Functor Columns where
  fmap f c = Columns {
    account = f (account c)
    , drCr = f (drCr c)
    , commodity = f (commodity c)
    , quantity = f (quantity c)
    }

instance Applicative Columns where
  pure a = Columns a a a a
  fn <*> fa = Columns {
    account = (account fn) (account fa)
    , drCr = (drCr fn) (drCr fa)
    , commodity = (commodity fn) (commodity fa)
    , quantity = (quantity fn) (quantity fa)
     }

data PreSpec = PreSpec {
  _justification :: R.Justification
  , _padSpec :: Chunk.TextSpec
  , bits :: [Chunk.Bit] }

type PreSpecMap = NM.NestedMap L.SubAccountName (Columns PreSpec)

-- | Takes a list of triples from bottomLineChunks and creates three
-- Cells, one each for DrCr, Commodity, and Qty.
bottomLineBalCells ::
  Chunk.TextSpec -- ^ Fill colors
  -> [(Chunk.Bit, Chunk.Bit, Chunk.Bit)]
  -> (PreSpec, PreSpec, PreSpec)
bottomLineBalCells spec ts = (mkSpec dc, mkSpec ct, mkSpec qt) where
  mkSpec ls = PreSpec R.LeftJustify spec ls
  (dc, ct, qt) = foldr f ([], [], []) ts
  f (da, ca, qa) (d, c, q) = (da:d, ca:c, qa:q)


type IsEven = Bool

-- | Returns a triple (x, y, z), where x is the DrCr chunk, y is the
-- commodity chunk, and z is the qty chunk.
bottomLineBalChunks ::
  O.Options
  -> IsEven
  -> (L.Commodity, Bal.BottomLine)
  -> (Chunk.Bit, Chunk.Bit, Chunk.Bit)
bottomLineBalChunks os isEven (comm, bl) = (dc, cty, qty) where
  dc = Chunk.bit ts dcTxt
  cty = Chunk.bit ts ctyTxt
  qty = Chunk.bit ts qtyTxt
  ctyTxt = L.text (L.Delimited (X.singleton ':') (L.textList comm))
  (ts, dcTxt, qtyTxt) = case bl of
    Bal.Zero -> let
      getTs = if isEven then C.evenZero else C.oddZero
      dcT = X.pack "--"
      qtyT = dcT
      in (getTs . O.drCrColors $ os, dcT, qtyT)
    Bal.NonZero clm -> let
      (getTs, dcT) = case Bal.drCr clm of
        L.Debit ->
          (if isEven then C.evenDebit else C.oddDebit,
           X.pack "Dr")
        L.Credit ->
          (if isEven then C.evenCredit else C.oddCredit,
           X.pack "Cr")
      qTxt = (O.balanceFormat os) bl
      in (getTs . O.drCrColors $ os, dcT, qTxt)


fillTextSpec ::
  O.Options
  -> IsEven
  -> Chunk.TextSpec
fillTextSpec os isEven = let
  getTs = if isEven then C.evenColors else C.oddColors
  in getTs . O.baseColors $ os
  

bottomLineCells ::
  O.Options
  -> IsEven
  -> SummedBal
  -> (PreSpec, PreSpec, PreSpec)
bottomLineCells os isEven mayBal = let
  fill = fillTextSpec os isEven
  tsZero = if isEven
           then C.evenZero . O.drCrColors $ os
           else C.oddZero . O.drCrColors $ os
  zeroSpec =
    PreSpec R.LeftJustify
    tsZero [Chunk.bit tsZero (X.pack "--")]
  zeroSpecs = (zeroSpec, zeroSpec, zeroSpec)
  in case S.getOption . unSummedBal $ mayBal of
    Nothing -> zeroSpecs
    Just bal -> bottomLineBalCells fill
                . map (bottomLineBalChunks os isEven)
                . M.assocs
                . Bal.unBalance
                $ bal

padding :: Int
padding = 2

accountPreSpec ::
  O.Options
  -> IsEven
  -> Int
  -> L.SubAccountName
  -> PreSpec
accountPreSpec os isEven lvl acct = PreSpec j ts [bit] where
  j = R.LeftJustify
  ts = if isEven
       then C.evenColors . O.baseColors $ os
       else C.oddColors . O.baseColors $ os
  bit = Chunk.bit ts txt where
    txt = pad `X.append` (L.text acct)
    pad = X.replicate (padding * lvl) (X.singleton ' ')


makePreSpec ::
  O.Options
  -> [(L.SubAccountName, (SummedBal, Bool))]
  -> L.SubAccountName
  -> (SummedBal, Bool)
  -> Columns PreSpec
makePreSpec os ps a (mayBal, isEven) = Columns act dc com qt where
  lvl = length ps + 1
  act = accountPreSpec os isEven lvl a
  (dc, com, qt) = bottomLineCells os isEven mayBal

traverser ::
  O.Options
  -> [(L.SubAccountName, (SummedBal, Bool))]
  -> L.SubAccountName
  -> (SummedBal, Bool)
  -> a
  -> Id.Identity (Maybe (Columns PreSpec))
traverser os hist a mayBal _ =
  return (Just $ makePreSpec os hist a mayBal)

makePreSpecMap ::
  O.Options
  -> (SummedWithIsEven, TotalBal)
  -> (PreSpecMap, TotalBal)
makePreSpecMap os (sb, tb) = (cim, tb) where
  cim = Id.runIdentity (NM.traverseWithTrail (traverser os) sb)

-- Step 7
makeTotalCells ::
  O.Options
  -> SummedBal
  -> Columns PreSpec
makeTotalCells os mayBal = Columns act dc com qt where
  act = accountPreSpec os True 0 tot
  tot = L.SubAccountName $ L.TextNonEmpty 'T' (X.pack "otal")
  (dc, com, qt) = bottomLineCells os True mayBal

makeColumnList :: O.Options -> (PreSpecMap, TotalBal) -> [Columns PreSpec]
makeColumnList os (cim, tb) = totCols : restCols where
  totCols = makeTotalCells os tb
  restCols = Fdbl.toList cim


-- Step 8

-- | When given a list of columns, determine the widest row in each
-- column.
maxWidths :: [Columns PreSpec] -> Columns R.Width
maxWidths = Fdbl.foldl' maxWidthPerColumn (pure (R.Width 0))

-- | Applied to a Columns of PreSpec and a Colums of widths, return a
-- Columns that has the wider of the two values.
maxWidthPerColumn ::
  Columns R.Width
  -> Columns PreSpec
  -> Columns R.Width
maxWidthPerColumn w p = f <$> w <*> p where
  f old new = max old (maximum . map Chunk.bitWidth . bits $ new)
  
-- | Changes a single set of Columns to a set of ColumnSpec of the
-- given width.
preSpecToSpec ::
  Columns R.Width
  -> Columns PreSpec
  -> Columns R.ColumnSpec
preSpecToSpec ws p = f <$> ws <*> p where
  f width (PreSpec j ps bs) = R.ColumnSpec j width ps bs

resizeColumnsInList :: [Columns PreSpec] -> [Columns R.ColumnSpec]
resizeColumnsInList cs = map (preSpecToSpec w) cs where
  w = maxWidths cs


-- Step 9
widthSpacerAcct :: Int
widthSpacerAcct = 4

widthSpacerDrCr :: Int
widthSpacerDrCr = 1

widthSpacerCommodity :: Int
widthSpacerCommodity = 1

colsToBits ::
  O.Options
  -> IsEven
  -> Columns R.ColumnSpec
  -> [Chunk.Bit]
colsToBits os isEven (Columns a dc c q) = let
  fillSpec = if isEven
             then C.evenColors . O.baseColors $ os
             else C.oddColors . O.baseColors $ os
  spacer w = R.ColumnSpec j (Chunk.Width w) fillSpec []
  j = R.LeftJustify
  cs = a
       : spacer widthSpacerAcct
       : dc
       : spacer widthSpacerDrCr
       : c
       : spacer widthSpacerCommodity
       : q
       : []
  in R.row cs

colsListToBits ::
  O.Options
  -> [Columns R.ColumnSpec]
  -> [[Chunk.Bit]]
colsListToBits os = zipWith f bools where
  f b c = colsToBits os b c
  bools = iterate not True


-- Tie it all together

report ::
  O.Options
  -> [L.Box Ly.LibertyMeta]
  -> [L.PricePoint]
  -> Ex.Exceptional X.Text [[Chunk.Bit]]
report os ps rs = do
  pcs <- converter os rs ps
  return
    . colsListToBits os
    . resizeColumnsInList
    . makeColumnList os
    . makePreSpecMap os
    . makeSummedWithIsEven
    . sumBalances
    . rawBalances
    . toFlatMap os
    $ pcs
