{-# LANGUAGE OverloadedStrings #-}

module Penny.Cabin.Posts.Parser (State(..),
                                 allSpecs,
                                 showError) where

import Control.Applicative ((<$>), pure, (<*>),
                            Applicative)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toLower)
import qualified Data.Foldable as Fdbl
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg as MA

import qualified Penny.Cabin.Parsers as P
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Types as Ty
import qualified Penny.Cabin.Options as CO
import qualified Penny.Liberty as Ly
import qualified Penny.Steel.Expressions as Exp
import qualified Penny.Steel.Predtree as Pt
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Predicates as Pd
import qualified Penny.Shield as S
import qualified Text.Matchers as M

data State = State
  { sensitive :: M.CaseSensitive
  , factory :: L.Factory
  , tokens :: [Exp.Token (L.Box Ly.LibertyMeta)]
  , postFilter :: [Ly.PostFilterFn]
  , fields :: F.Fields Bool
  , width :: Ty.ReportWidth
  , showZeroBalances :: CO.ShowZeroBalances
  , exprDesc :: Exp.ExprDesc
  }

allSpecs
  :: S.Runtime -> [MA.OptSpec (State -> Ex.Exceptional Error State)]
allSpecs rt =
  operand rt
  ++ boxFilters
  ++ parsePostFilter
  ++ (map (fmap (pure .)) matcherSelect)
  ++ (map (fmap (pure .)) caseSelect)
  ++ (map (fmap (pure .)) operator)
  ++ map (fmap (pure .)) parseExprType
  ++ [ parseWidth
     , showField
     , hideField
     , fmap (pure .) showAllFields
     , fmap (pure .) hideAllFields
     , fmap (pure .) parseZeroBalances
     ]


operand
  :: S.Runtime
  -> [MA.OptSpec (State -> Ex.Exceptional Error State)]
operand rt = map (fmap f) (Ly.operandSpecs (S.currentTime rt))
  where
    f lyFn st = do
      let cs = sensitive st
          fty = factory st
      g <- Ex.mapException EOperandError $ lyFn cs fty
      let g' = Pt.boxPdct L.boxPostFam g
          ts' = tokens st ++ [Exp.operand g']
      return $ st { tokens = ts' }


data Error
  = EOperandError Ly.OperandError
  | EBadComparer Text
  | EBadInt Text
  | EHeadTail Ly.BadHeadTailError
  | EBadFieldError Text BadFieldError
  -- ^ The first argument is the text supplied on the command line;
  -- the second is the error.

showError :: Error -> String
showError = undefined

-- | Processes a option for box-level serials.
optBoxSerial
  :: String
  -- ^ Serial name

  -> (Ly.LibertyMeta -> Int)
  -- ^ Pulls the serial from the PostMeta

  -> C.OptSpec (State -> Ex.Exceptional Error State)

optBoxSerial nm f = C.OptSpec [nm] "" (C.TwoArg g)
  where
    g a1 a2 st = do
      cmp <- Ex.fromMaybe (EBadComparer (X.pack a1))
             $ Ly.parseComparer a1
      i <- Ex.fromMaybe (EBadInt (X.pack a2))
           $ Ly.parseInt a2
      let (cmpDesc, cmpFn) = Pd.descComp cmp
          desc = "serial " <> X.pack nm <> " is " <> cmpDesc
                 <> " " <> X.pack (show i)
          pdct box = (f . L.boxMeta $ box) `cmpFn` i
          opnd = Pt.operand desc pdct
          tok = Exp.operand opnd
      return $ st { tokens = tokens st ++ [tok] }

optFilteredNum :: C.OptSpec (State -> Ex.Exceptional Error State)
optFilteredNum = optBoxSerial "filtered" f
  where
    f = L.forward . Ly.unFilteredNum . Ly.filteredNum

optRevFilteredNum :: C.OptSpec (State -> Ex.Exceptional Error State)
optRevFilteredNum = optBoxSerial "revFiltered" f
  where
    f = L.backward . Ly.unFilteredNum . Ly.filteredNum

optSortedNum :: C.OptSpec (State -> Ex.Exceptional Error State)
optSortedNum = optBoxSerial "sorted" f
  where
    f = L.forward . Ly.unSortedNum . Ly.sortedNum

optRevSortedNum :: C.OptSpec (State -> Ex.Exceptional Error State)
optRevSortedNum = optBoxSerial "revSorted" f
  where
    f = L.backward . Ly.unSortedNum . Ly.sortedNum

boxFilters :: [C.OptSpec (State -> Ex.Exceptional Error State)]
boxFilters =
  [ optFilteredNum
  , optRevFilteredNum
  , optSortedNum
  , optRevSortedNum
  ]


parsePostFilter :: [C.OptSpec (State -> Ex.Exceptional Error State)]
parsePostFilter = [fmap f optH, fmap f optT]
  where
    (optH, optT) = Ly.postFilterSpecs
    f exc = case exc of
      Ex.Exception e -> const $ Ex.throw (EHeadTail e)
      Ex.Success pf ->
        \st -> return $ st { postFilter = postFilter st ++ [pf] }


matcherSelect :: [C.OptSpec (State -> State)]
matcherSelect = map (fmap f) Ly.matcherSelectSpecs
  where
    f mf st = st { factory = mf }


caseSelect :: [C.OptSpec (State -> State)]
caseSelect = map (fmap f) Ly.caseSelectSpecs
  where
    f cs st = st { sensitive = cs }

operator :: [C.OptSpec (State -> State)]
operator = map (fmap f) Ly.operatorSpecs
  where
    f oo st = st { tokens = tokens st ++ [oo] }

parseWidth :: C.OptSpec (State -> Ex.Exceptional Error State)
parseWidth = C.OptSpec ["width"] "" (C.OneArg f)
  where
    f a1 st = do
      i <- Ex.fromMaybe (EBadInt (X.pack a1))
           $ Ly.parseInt a1
      return $ st { width = Ty.ReportWidth i }

parseField :: String -> Ex.Exceptional Error (F.Fields Bool)
parseField str =
  let lower = map toLower str
      checkField s =
        if (map toLower s) == lower
        then (s, True)
        else (s, False)
      flds = checkField <$> F.fieldNames
  in Ex.mapException (EBadFieldError (X.pack str)) $ checkFields flds


-- | Turns a field on if it is True.
fieldOn ::
  F.Fields Bool
  -- ^ Fields as seen so far

  -> F.Fields Bool
  -- ^ Record that should have one True element indicating a field
  -- name seen on the command line; other elements should be False

  -> F.Fields Bool
  -- ^ Fields as seen so far, with new field added

fieldOn old new = (||) <$> old <*> new

-- | Turns off a field if it is True.
fieldOff ::
  F.Fields Bool
  -- ^ Fields seen so far

  -> F.Fields Bool
  -- ^ Record that should have one True element indicating a field
  -- name seen on the command line; other elements should be False

  -> F.Fields Bool
  -- ^ Fields as seen so far, with new field added

fieldOff old new = f <$> old <*> new
  where
    f o False = o
    f _ True = False

showField :: C.OptSpec (State -> Ex.Exceptional Error State)
showField = C.OptSpec ["show"] "" (C.OneArg f)
  where
    f a1 st = do
      fl <- parseField a1
      let newFl = fieldOn (fields st) fl
      return $ st { fields = newFl }

hideField :: C.OptSpec (State -> Ex.Exceptional Error State)
hideField = C.OptSpec ["hide"] "" (C.OneArg f)
  where
    f a1 st = do
      fl <- parseField a1
      let newFl = fieldOff (fields st) fl
      return $ st { fields = newFl }

showAllFields :: C.OptSpec (State -> State)
showAllFields = C.OptSpec ["show-all"] "" (C.NoArg f)
  where
    f st = st {fields = pure True}

hideAllFields :: C.OptSpec (State -> State)
hideAllFields = C.OptSpec ["hide-all"] "" (C.NoArg f)
  where
    f st = st {fields = pure False}

parseZeroBalances :: C.OptSpec (State -> State)
parseZeroBalances = fmap f P.zeroBalances
  where
    f szb st = st { showZeroBalances = szb }

parseExprType :: [C.OptSpec (State -> State)]
parseExprType = map (fmap f) [Ly.parseInfix, Ly.parseRPN]
  where
    f d st = st { exprDesc = d }

data BadFieldError
  = NoMatchingFields
  | MultipleMatchingFields [Text]
  deriving Show

-- | Checks the fields with the True value to ensure there is only one.
checkFields ::
  F.Fields (String, Bool)
  -> Ex.Exceptional BadFieldError (F.Fields Bool)
checkFields fs =
  let f (s, b) ls = if b then s:ls else ls
  in case Fdbl.foldr f [] fs of
    [] -> Ex.throw NoMatchingFields
    _:[] -> return (snd <$> fs)
    ms -> Ex.throw . MultipleMatchingFields . map X.pack $ ms


