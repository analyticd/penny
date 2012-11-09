module Penny.Cabin.Posts.Parser (State(..),
                                 parseOptions) where

import Control.Applicative ((<$>), pure, many, (<*>),
                            Applicative)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toLower)
import Data.List (intersperse)
import qualified Data.Foldable as Fdbl
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Parsers as P
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Types as Ty
import qualified Penny.Cabin.Options as CO
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as Exp
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Text.Matchers.Text as M

data State = State {
  sensitive :: M.CaseSensitive
  , factory :: L.Factory
  , tokens :: [Ly.Token (L.Box Ly.LibertyMeta -> Bool)]
  , postFilter :: [Ly.PostFilterFn]
  , fields :: F.Fields Bool
  , colorPref :: CC.Colors
  , drCrColors :: PC.DrCrColors
  , baseColors :: PC.BaseColors
  , width :: Ty.ReportWidth
  , showZeroBalances :: CO.ShowZeroBalances
  }

-- | Parses the command line from the first word remaining up until,
-- but not including, the first non-option argment.
parseOptions ::
  Parser (S.Runtime
          -> State
          -> State)
parseOptions = f <$> many parseOption where
  f ls rt st =
    let ls' = map (\fn -> fn rt) ls
    in foldl (flip (.)) id ls' st


parseOption ::
  Parser (S.Runtime
          -> State
          -> State)
parseOption = C.parseOption ls
  where
    ls = operand ++ map wrap others ++ [color]
    wrap = fmap (\fn _ st -> fn st)
    others =
      boxFilters
      ++ parsePostFilter
      ++ matcherSelect
      ++ caseSelect
      ++ operator
      ++ [ background
         , parseWidth
         , showField
         , hideField
         , showAllFields
         , hideAllFields ]
      ++ parseZeroBalances


operand :: [C.OptSpec (S.Runtime
                       -> State
                       -> State)]
operand = map (fmap f) Ly.operandSpecs
  where
    f lyFn rt st =
      let dt = S.currentTime rt
          cs = sensitive st
          fty = factory st
          (Exp.Operand g) = lyFn dt cs fty
          g' = g . L.boxPostFam
          ts' = tokens st ++ [Exp.TokOperand g']
      in st { tokens = ts' }

-- | Processes a option for box-level serials.
optBoxSerial ::
  [String]
  -- ^ Long options
  
  -> [Char]
  -- ^ Short options
  
  -> (Ly.LibertyMeta -> Int)
  -- ^ Pulls the serial from the PostMeta
  
  -> C.OptSpec (State -> State)

optBoxSerial ls ss f = C.OptSpec ls ss (C.TwoArg g)
  where
    g a1 a2 st =
      let cmp = Ly.parseComparer a1
          i = Ly.parseInt a2
          h box =
            let ser = f . L.boxMeta $ box
            in ser `cmp` i
          tok = Exp.TokOperand h
      in st { tokens = tokens st ++ [tok] }

optFilteredNum :: C.OptSpec (State -> State)
optFilteredNum = optBoxSerial ["filtered"] "" f
  where
    f = L.forward . Ly.unFilteredNum . Ly.filteredNum

optRevFilteredNum :: C.OptSpec (State -> State)
optRevFilteredNum = optBoxSerial ["revFiltered"] "" f
  where
    f = L.backward . Ly.unFilteredNum . Ly.filteredNum

optSortedNum :: C.OptSpec (State -> State)
optSortedNum = optBoxSerial ["sorted"] "" f
  where
    f = L.forward . Ly.unSortedNum . Ly.sortedNum

optRevSortedNum :: C.OptSpec (State -> State)
optRevSortedNum = optBoxSerial ["revSorted"] "" f
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
    f pf st = st { postFilter = postFilter st ++ [pf] }

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

color :: C.OptSpec (S.Runtime -> State -> State)
color = fmap f P.color
  where
    f pref rt st = st { colorPref = CO.autoColors pref rt }


background :: C.OptSpec (State -> State)
background = fmap f P.background
  where
    f (dc, bc) st = st { drCrColors = dc
                       , baseColors = bc }


parseWidth :: C.OptSpec (State -> State)
parseWidth = C.OptSpec ["width"] "" (C.OneArg f)
  where
    f a1 st = st { width = Ty.ReportWidth (Ly.parseInt a1) }

showField :: C.OptSpec (State -> State)
showField = C.OptSpec ["show"] "" (C.OneArg f)
  where
    f a1 st =
      let fl = parseField a1
          newFl = fieldOn (fields st) fl
      in st { fields = newFl }

hideField :: C.OptSpec (State -> State)
hideField = C.OptSpec ["hide"] "" (C.OneArg f)
  where
    f a1 st =
      let fl = parseField a1
          newFl = fieldOff (fields st) fl
      in st { fields = newFl }


showAllFields :: C.OptSpec (State -> State)
showAllFields = C.OptSpec ["show-all"] "" (C.NoArg f)
  where
    f st = st {fields = pure True}

hideAllFields :: C.OptSpec (State -> State)
hideAllFields = C.OptSpec ["hide-all"] "" (C.NoArg f)
  where
    f st = st {fields = pure False}

parseZeroBalances :: [C.OptSpec (State -> State)]
parseZeroBalances = map (fmap f) P.zeroBalances
  where
    f szb st = st { showZeroBalances = szb }

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

parseField :: String -> (F.Fields Bool)
parseField str =
  let lower = map toLower str
      checkField s =
        if (map toLower s) == lower
        then (s, True)
        else (s, False)
      flds = checkField <$> fieldNames
      err e = case e of
        NoMatchingFieldName ->
          Ly.abort $ "no matching field name: " ++ str
        MultipleMatchingFieldNames ns ->
          Ly.abort $ "multiple field names match: "
          ++ str ++ " matches: "
          ++ (concat . intersperse ", " $ ns)
  in Ex.resolve err (checkFields flds)


data CheckFieldsError =
  NoMatchingFieldName
  | MultipleMatchingFieldNames [String]


-- | Checks the fields with the True value to ensure there is only one.
checkFields ::
  F.Fields (String, Bool)
  -> Ex.Exceptional CheckFieldsError (F.Fields Bool)
checkFields fs =
  let f (s, b) ls = if b then s:ls else ls
  in case Fdbl.foldr f [] fs of
    [] -> Ex.throw NoMatchingFieldName
    _:[] -> return (snd <$> fs)
    ls -> Ex.throw . MultipleMatchingFieldNames $ ls



fieldNames :: F.Fields String
fieldNames = F.Fields {
  F.globalTransaction = "globalTransaction"
  , F.revGlobalTransaction = "revGlobalTransaction"
  , F.globalPosting = "globalPosting"
  , F.revGlobalPosting = "revGlobalPosting"
  , F.fileTransaction = "fileTransaction"
  , F.revFileTransaction = "revFileTransaction"
  , F.filePosting = "filePosting"
  , F.revFilePosting = "revFilePosting"
  , F.filtered = "filtered"
  , F.revFiltered = "revFiltered"
  , F.sorted = "sorted"
  , F.revSorted = "revSorted"
  , F.visible = "visible"
  , F.revVisible = "revVisible"
  , F.lineNum = "lineNum"
  , F.date = "date"
  , F.flag = "flag"
  , F.number = "number"
  , F.payee = "payee"
  , F.account = "account"
  , F.postingDrCr = "postingDrCr"
  , F.postingCmdty = "postingCmdty"
  , F.postingQty = "postingQty"
  , F.totalDrCr = "totalDrCr"
  , F.totalCmdty = "totalCmdty"
  , F.totalQty = "totalQty"
  , F.tags = "tags"
  , F.memo = "memo"
  , F.filename = "filename" }
