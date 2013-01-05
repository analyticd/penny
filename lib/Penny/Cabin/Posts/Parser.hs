module Penny.Cabin.Posts.Parser (State(..),
                                 allSpecs) where

import Control.Applicative ((<$>), pure, (<*>),
                            Applicative)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toLower)
import qualified Data.Foldable as Fdbl
import qualified System.Console.MultiArg.Combinator as C
import qualified System.Console.MultiArg as MA

import qualified Penny.Cabin.Parsers as P
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Types as Ty
import qualified Penny.Cabin.Options as CO
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as Exp
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Text.Matchers.Text as M

data State = State
  { sensitive :: M.CaseSensitive
  , factory :: L.Factory
  , tokens :: [Ly.Token (L.Box Ly.LibertyMeta -> Bool)]
  , postFilter :: [Ly.PostFilterFn]
  , fields :: F.Fields Bool
  , width :: Ty.ReportWidth
  , showZeroBalances :: CO.ShowZeroBalances
  , showHelp :: Bool
  }

allSpecs
  :: S.Runtime -> [MA.OptSpec (State -> Ex.Exceptional String State)]
allSpecs rt =
  operand rt
  ++ boxFilters
  ++ parsePostFilter
  ++ matcherSelect
  ++ caseSelect
  ++ operator
  ++ [ parseWidth
     , showField
     , hideField
     , showAllFields
     , hideAllFields
     , parseZeroBalances
     , optHelp
     ]


operand
  :: S.Runtime
  -> [MA.OptSpec (State -> Ex.Exceptional String State)]
operand rt = map (fmap f) (Ly.operandSpecs (S.currentTime rt))
  where
    f lyFn st = do
      let cs = sensitive st
          fty = factory st
      (Exp.Operand g) <- lyFn cs fty
      let g' = g . L.boxPostFam
          ts' = tokens st ++ [Exp.TokOperand g']
      return $ st { tokens = ts' }


-- | Processes a option for box-level serials.
optBoxSerial ::
  [String]
  -- ^ Long options

  -> [Char]
  -- ^ Short options

  -> (Ly.LibertyMeta -> Int)
  -- ^ Pulls the serial from the PostMeta

  -> C.OptSpec (State -> Ex.Exceptional String State)

optBoxSerial ls ss f = C.OptSpec ls ss (C.TwoArg g)
  where
    g a1 a2 st = do
      cmp <- Ly.parseComparer a1
      i <- Ly.parseInt a2
      let h box =
            let ser = f . L.boxMeta $ box
            in ser `cmp` i
          tok = Exp.TokOperand h
      return $ st { tokens = tokens st ++ [tok] }

optFilteredNum :: C.OptSpec (State -> Ex.Exceptional String State)
optFilteredNum = optBoxSerial ["filtered"] "" f
  where
    f = L.forward . Ly.unFilteredNum . Ly.filteredNum

optRevFilteredNum :: C.OptSpec (State -> Ex.Exceptional String State)
optRevFilteredNum = optBoxSerial ["revFiltered"] "" f
  where
    f = L.backward . Ly.unFilteredNum . Ly.filteredNum

optSortedNum :: C.OptSpec (State -> Ex.Exceptional String State)
optSortedNum = optBoxSerial ["sorted"] "" f
  where
    f = L.forward . Ly.unSortedNum . Ly.sortedNum

optRevSortedNum :: C.OptSpec (State -> Ex.Exceptional String State)
optRevSortedNum = optBoxSerial ["revSorted"] "" f
  where
    f = L.backward . Ly.unSortedNum . Ly.sortedNum

boxFilters :: [C.OptSpec (State -> Ex.Exceptional String State)]
boxFilters =
  [ optFilteredNum
  , optRevFilteredNum
  , optSortedNum
  , optRevSortedNum
  ]


parsePostFilter :: [C.OptSpec (State -> Ex.Exceptional String State)]
parsePostFilter = [fmap f optH, fmap f optT]
  where
    (optH, optT) = Ly.postFilterSpecs
    f exc = case exc of
      Ex.Exception s -> const $ Ex.throw s
      Ex.Success pf ->
        let g st = return $ st { postFilter = postFilter st ++ [pf] }
        in g


matcherSelect :: Applicative f => [C.OptSpec (State -> f State)]
matcherSelect = map (fmap f) Ly.matcherSelectSpecs
  where
    f mf st = pure $ st { factory = mf }


caseSelect :: Applicative f => [C.OptSpec (State -> f State)]
caseSelect = map (fmap f) Ly.caseSelectSpecs
  where
    f cs st = pure $ st { sensitive = cs }

operator :: Applicative f => [C.OptSpec (State -> f State)]
operator = map (fmap f) Ly.operatorSpecs
  where
    f oo st = pure $ st { tokens = tokens st ++ [oo] }

parseWidth :: C.OptSpec (State -> Ex.Exceptional String State)
parseWidth = C.OptSpec ["width"] "" (C.OneArg f)
  where
    f a1 st = do
      i <- Ly.parseInt a1
      return $ st { width = Ty.ReportWidth i }

parseField :: String -> Ex.Exceptional String (F.Fields Bool)
parseField str =
  let lower = map toLower str
      checkField s =
        if (map toLower s) == lower
        then (s, True)
        else (s, False)
      flds = checkField <$> F.fieldNames
  in checkFields flds


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

showField :: C.OptSpec (State -> Ex.Exceptional String State)
showField = C.OptSpec ["show"] "" (C.OneArg f)
  where
    f a1 st = do
      fl <- parseField a1
      let newFl = fieldOn (fields st) fl
      return $ st { fields = newFl }

hideField :: C.OptSpec (State -> Ex.Exceptional String State)
hideField = C.OptSpec ["hide"] "" (C.OneArg f)
  where
    f a1 st = do
      fl <- parseField a1
      let newFl = fieldOff (fields st) fl
      return $ st { fields = newFl }

showAllFields :: Applicative f => C.OptSpec (State -> f State)
showAllFields = C.OptSpec ["show-all"] "" (C.NoArg f)
  where
    f st = pure $ st {fields = pure True}

hideAllFields :: Applicative f => C.OptSpec (State -> f State)
hideAllFields = C.OptSpec ["hide-all"] "" (C.NoArg f)
  where
    f st = pure $ st {fields = pure False}

optHelp :: Applicative f => C.OptSpec (State -> f State)
optHelp = fmap f P.help
  where
    f _ st = pure $ st { showHelp = True }

parseZeroBalances :: Applicative f => C.OptSpec (State -> f State)
parseZeroBalances = fmap f P.zeroBalances
  where
    f szb st = pure $ st { showZeroBalances = szb }

-- | Checks the fields with the True value to ensure there is only one.
checkFields ::
  F.Fields (String, Bool)
  -> Ex.Exceptional String (F.Fields Bool)
checkFields fs =
  let f (s, b) ls = if b then s:ls else ls
  in case Fdbl.foldr f [] fs of
    [] -> Ex.throw "no matching field names"
    _:[] -> return (snd <$> fs)
    _ -> Ex.throw "multiple matching field names"


