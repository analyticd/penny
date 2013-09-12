{-# LANGUAGE OverloadedStrings #-}

module Penny.Cabin.Posts.Parser
  ( State(..)
  , allSpecs
  , Error
  , VerboseFilter(..)
  , ShowExpression(..)
  ) where

import Control.Applicative ((<$>), pure, (<*>),
                            Applicative)
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
import qualified Data.Prednote.Expressions as Exp
import qualified Data.Prednote.Pdct as Pt
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Text.Matchers as M

newtype VerboseFilter = VerboseFilter { unVerboseFilter :: Bool }
  deriving (Eq, Show)

newtype ShowExpression = ShowExpression { unShowExpression :: Bool }
  deriving (Eq, Show)

data State = State
  { sensitive :: M.CaseSensitive
  , factory :: L.Factory
  , tokens :: [Exp.Token (Ly.LibertyMeta, L.Posting)]
  , postFilter :: [Ly.PostFilterFn]
  , fields :: F.Fields Bool
  , width :: Ty.ReportWidth
  , showZeroBalances :: CO.ShowZeroBalances
  , exprDesc :: Exp.ExprDesc
  , verboseFilter :: VerboseFilter
  , showExpression :: ShowExpression
  }

type Error = X.Text

allSpecs
  :: S.Runtime -> [MA.OptSpec (State -> Either Error State)]
allSpecs rt =
  operand rt
  ++ listToErr boxFilters
  ++ listToErr parsePostFilter
  ++ listToErr matcherSelect
  ++ listToErr caseSelect
  ++ listToErr operator
  ++ listToErr parseExprType
  ++ listToErr [ parseWidth
     , showField
     , hideField
     , showAllFields
     , hideAllFields
     , parseZeroBalances
     , parseShowExpression
     , parseVerboseFilter
     ]
  where
    listToErr = map (fmap (fmap return))


operand
  :: S.Runtime
  -> [MA.OptSpec (State -> Either Error State)]
operand rt = map (fmap f) (Ly.operandSpecs (S.currentTime rt))
  where
    f lyFn st = do
      let cs = sensitive st
          fty = factory st
      g <- lyFn cs fty
      let g' = Pt.boxPdct snd g
          ts' = tokens st ++ [Exp.operand g']
      return $ st { tokens = ts' }


-- | Processes a option for box-level serials.
optBoxSerial
  :: String
  -- ^ Serial name

  -> (Ly.LibertyMeta -> Int)
  -- ^ Pulls the serial from the PostMeta

  -> C.OptSpec (State -> State)

optBoxSerial nm f = C.OptSpec [nm] "" (C.TwoArg g)
  where
    g a1 a2 = do
      i <- Ly.parseIntMA a2
      let getPd = Pt.compareBy (X.pack . show $ i)
                  ("serial " <> X.pack nm) cmp
          cmp l = compare (f . fst $ l) i
      pd <- Ly.parseComparer a1 getPd
      let tok = Exp.operand pd
      return $ \st -> st { tokens = tokens st ++ [tok] }

optFilteredNum :: C.OptSpec (State -> State)
optFilteredNum = optBoxSerial "filtered" f
  where
    f = L.forward . Ly.unFilteredNum . Ly.filteredNum

optRevFilteredNum :: C.OptSpec (State -> State)
optRevFilteredNum = optBoxSerial "revFiltered" f
  where
    f = L.backward . Ly.unFilteredNum . Ly.filteredNum

optSortedNum :: C.OptSpec (State -> State)
optSortedNum = optBoxSerial "sorted" f
  where
    f = L.forward . Ly.unSortedNum . Ly.sortedNum

optRevSortedNum :: C.OptSpec (State -> State)
optRevSortedNum = optBoxSerial "revSorted" f
  where
    f = L.backward . Ly.unSortedNum . Ly.sortedNum

boxFilters :: [C.OptSpec (State -> State)]
boxFilters =
  [ optFilteredNum
  , optRevFilteredNum
  , optSortedNum
  , optRevSortedNum
  ]


parsePostFilter :: [C.OptSpec (State -> State)]
parsePostFilter = [fmap f optH, fmap f optT]
  where
    (optH, optT) = Ly.postFilterSpecs
    f pff st = st { postFilter = postFilter st ++ [pff] }


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

parseWidth :: C.OptSpec (State -> State)
parseWidth = C.OptSpec ["width"] "" (C.OneArg f)
  where
    f a1 = do
      i <- Ly.parseIntMA a1
      return $ \st -> st { width = Ty.ReportWidth i }

parseField :: String -> Either MA.InputError (F.Fields Bool)
parseField str =
  let lower = map toLower str
      checkField s =
        if (map toLower s) == lower
        then (s, True)
        else (s, False)
      flds = checkField <$> F.fieldNames
  in case checkFields flds of
      Left e -> case e of
        NoMatchingFields -> Left . MA.ErrorMsg
          $ "no matching fields"
        MultipleMatchingFields ts -> Left . MA.ErrorMsg
          $ "multiple matching fields: "
            <> X.unpack mtchs <> "\n"
          where
            mtchs = X.intercalate " "
                    . map (\x -> "\"" <> x <> "\"")
                    $ ts
      Right g -> return g


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

showField :: C.OptSpec (State -> State)
showField = C.OptSpec ["show"] "" (C.OneArg f)
  where
    f a1 = do
      fl <- parseField a1
      return $ \st ->
        let newFl = fieldOn (fields st) fl
        in st { fields = newFl }


hideField :: C.OptSpec (State -> State)
hideField = C.OptSpec ["hide"] "" (C.OneArg f)
  where
    f a1 = do
      fl <- parseField a1
      return $ \st ->
        let newFl = fieldOff (fields st) fl
        in st { fields = newFl }

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

parseShowExpression :: C.OptSpec (State -> State)
parseShowExpression = fmap f Ly.showExpression
  where
    f _ st = st { showExpression = ShowExpression True }

parseVerboseFilter :: C.OptSpec (State -> State)
parseVerboseFilter = fmap f Ly.verboseFilter
  where
    f _ st = st { verboseFilter = VerboseFilter True }

data BadFieldError
  = NoMatchingFields
  | MultipleMatchingFields [Text]
  deriving Show

-- | Checks the fields with the True value to ensure there is only one.
checkFields ::
  F.Fields (String, Bool)
  -> Either BadFieldError (F.Fields Bool)
checkFields fs =
  let f (s, b) ls = if b then s:ls else ls
  in case Fdbl.foldr f [] fs of
    [] -> Left NoMatchingFields
    _:[] -> return (snd <$> fs)
    ms -> Left . MultipleMatchingFields . map X.pack $ ms


