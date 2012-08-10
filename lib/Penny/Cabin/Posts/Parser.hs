module Penny.Cabin.Posts.Parser (State(..), parseOptions) where

import Control.Applicative ((<|>), (<$>), pure, many, (<*>),
                            Applicative)
import Control.Monad ((>=>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Char (toLower)
import qualified Data.Foldable as Fdbl
import qualified System.Console.MultiArg.Combinator as C
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Types as Ty
import qualified Penny.Cabin.Colors.DarkBackground as DB
import qualified Penny.Cabin.Colors.LightBackground as LB
import qualified Penny.Cabin.Options as CO
import qualified Penny.Copper as Cop
import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as Exp
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Text.Matchers.Text as M

data Error = BadColorName String
             | BadBackgroundArg String
             | BadWidthArg String
             | NoMatchingFieldName
             | MultipleMatchingFieldNames [String]
             | LibertyError Ly.Error
             | BadNumber String
             | BadComparator String
             deriving Show

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
          -> Cop.DefaultTimeZone
          -> Cop.RadGroup
          -> State
          -> Ex.Exceptional Error State)
parseOptions = f <$> many parseOption where
  f ls =
    let g rt dtz rg st =
          let ls' = map (\fn -> fn rt dtz rg) ls
          in (foldl (>=>) return ls') st
    in g


parseOption ::
  Parser (S.Runtime
          -> Cop.DefaultTimeZone
          -> Cop.RadGroup
          -> State
          -> Ex.Exceptional Error State)
parseOption =
  operand
  <|> wrap boxFilters
  <|> wrap parsePostFilter
  <|> wrap (impurify matcherSelect)
  <|> wrap (impurify caseSelect)
  <|> wrap (impurify operator)
  <|> (do { f <- color; return (\rt _ _ st -> f rt st)})
  <|> wrap background
  <|> wrap parseWidth
  <|> wrap showField
  <|> wrap hideField
  <|> wrap (impurify showAllFields)
  <|> wrap (impurify hideAllFields)
  <|> wrap (impurify parseShowZeroBalances)
  <|> wrap (impurify hideZeroBalances)
  where
    wrap p = do
      f <- p
      return (\_ _ _ st -> f st)

impurify ::
  (Functor f, Applicative m)
  => f (a -> b)
  -> f (a -> m b)
impurify = fmap (\g -> pure . g)

operand :: Parser (S.Runtime
                   -> Cop.DefaultTimeZone
                   -> Cop.RadGroup
                   -> State
                   -> Ex.Exceptional Error State)
operand = f <$> Ly.parseOperand
  where
    f lyFn rt dtz rg st =
      let dt = S.currentTime rt
          cs = sensitive st
          fty = factory st
      in case lyFn dt dtz rg cs fty of
        Ex.Exception e -> Ex.throw . LibertyError $ e
        Ex.Success (Exp.Operand g) ->
          let g' = g . L.boxPostFam
              ts' = tokens st ++ [Exp.TokOperand g']
          in return st { tokens = ts' }

-- | Processes a option for box-level serials.
optBoxSerial ::
  [String]
  -- ^ Long options
  
  -> [Char]
  -- ^ Short options
  
  -> (Ly.LibertyMeta -> Int)
  -- ^ Pulls the serial from the PostMeta
  
  -> Parser (State -> Ex.Exceptional Error State)

optBoxSerial ls ss f = parseOpt ls ss (C.TwoArg g)
  where
    g a1 a2 st = do
      cmp <- Ex.fromMaybe (BadComparator a1) (Ly.parseComparer a1)
      i <- parseInt a2
      let h box =
            let ser = f . L.boxMeta $ box
            in ser `cmp` i
          tok = Exp.TokOperand h
      return st { tokens = tokens st ++ [tok] }

optFilteredNum :: Parser (State -> Ex.Exceptional Error State)
optFilteredNum = optBoxSerial ["filtered"] "" f
  where
    f = L.forward . Ly.unFilteredNum . Ly.filteredNum

optRevFilteredNum :: Parser (State -> Ex.Exceptional Error State)
optRevFilteredNum = optBoxSerial ["revFiltered"] "" f
  where
    f = L.backward . Ly.unFilteredNum . Ly.filteredNum

optSortedNum :: Parser (State -> Ex.Exceptional Error State)
optSortedNum = optBoxSerial ["sorted"] "" f
  where
    f = L.forward . Ly.unSortedNum . Ly.sortedNum

optRevSortedNum :: Parser (State -> Ex.Exceptional Error State)
optRevSortedNum = optBoxSerial ["revSorted"] "" f
  where
    f = L.backward . Ly.unSortedNum . Ly.sortedNum

parseInt :: String -> Ex.Exceptional Error Int
parseInt s = case reads s of
  (i, ""):[] -> return i
  _ -> Ex.throw . BadNumber $ s

boxFilters :: Parser (State -> Ex.Exceptional Error State)
boxFilters =
  optFilteredNum
  <|> optRevFilteredNum
  <|> optSortedNum
  <|> optRevSortedNum


parsePostFilter :: Parser (State -> Ex.Exceptional Error State)
parsePostFilter = f <$> Ly.parsePostFilter
  where
    f ex st =
      case ex of
        Ex.Exception e -> Ex.throw . LibertyError $ e
        Ex.Success pf ->
          return st { postFilter = postFilter st ++ [pf] }

matcherSelect :: Parser (State -> State)
matcherSelect = f <$> Ly.parseMatcherSelect
  where
    f mf st = st { factory = mf }

caseSelect :: Parser (State -> State)
caseSelect = f <$> Ly.parseCaseSelect
  where
    f cs st = st { sensitive = cs }

operator :: Parser (State -> State)
operator = f <$> Ly.parseOperator
  where
    f oo st = st { tokens = tokens st ++ [oo] }

parseOpt :: [String] -> [Char] -> C.ArgSpec a -> Parser a
parseOpt ss cs a = C.parseOption [C.OptSpec ss cs a]

color :: Parser (S.Runtime -> State -> Ex.Exceptional Error State)
color = parseOpt ["color"] "" (C.OneArg f)
  where
    f a1 rt st = case pickColorArg rt a1 of
      Nothing -> Ex.throw . BadColorName $ a1
      Just c -> return (st { colorPref = c })

pickColorArg :: S.Runtime -> String -> Maybe CC.Colors
pickColorArg rt t
  | t == "yes" = Just CC.Colors8
  | t == "no" = Just CC.Colors0
  | t == "256" = Just CC.Colors256
  | t == "auto" = Just . CO.maxCapableColors $ rt
  | otherwise = Nothing

pickBackgroundArg :: String -> Maybe (PC.DrCrColors, PC.BaseColors)
pickBackgroundArg t
  | t == "light" = Just (LB.drCrColors, LB.baseColors)
  | t == "dark" = Just (DB.drCrColors, DB.baseColors)
  | otherwise = Nothing


background :: Parser (State -> Ex.Exceptional Error State)
background = parseOpt ["background"] "" (C.OneArg f)
  where
    f a1 st = case pickBackgroundArg a1 of
      Nothing -> Ex.throw . BadBackgroundArg $ a1
      Just (dc, bc) -> return (st { drCrColors = dc
                                  , baseColors = bc } )


parseWidth :: Parser (State -> Ex.Exceptional Error State)
parseWidth = parseOpt ["width"] "" (C.OneArg f)
  where
    f a1 st = case reads a1 of
      (i, ""):[] -> return (st { width = Ty.ReportWidth i })
      _ -> Ex.throw . BadWidthArg $ a1

showField :: Parser (State -> Ex.Exceptional Error State)
showField = parseOpt ["show"] "" (C.OneArg f)
  where
    f a1 st = do
      fl <- parseField a1
      let newFl = fieldOn (fields st) fl
      return st { fields = newFl }

hideField :: Parser (State -> Ex.Exceptional Error State)
hideField = parseOpt ["hide"] "" (C.OneArg f)
  where
    f a1 st = do
      fl <- parseField a1
      let newFl = fieldOff (fields st) fl
      return st { fields = newFl }

showAllFields :: Parser (State -> State)
showAllFields = parseOpt ["show-all"] "" (C.NoArg f)
  where
    f st = st {fields = pure True}

hideAllFields :: Parser (State -> State)
hideAllFields = parseOpt ["hide-all"] "" (C.NoArg f)
  where
    f st = st {fields = pure False}

parseShowZeroBalances :: Parser (State -> State)
parseShowZeroBalances = parseOpt opt "" (C.NoArg f)
  where
    opt = ["show-zero-balances"]
    f st = st {showZeroBalances = CO.ShowZeroBalances True }

hideZeroBalances :: Parser (State -> State)
hideZeroBalances = parseOpt ["hide-zero-balances"] "" (C.NoArg f)
  where
    f st = st {showZeroBalances = CO.ShowZeroBalances False }

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

parseField :: String -> Ex.Exceptional Error (F.Fields Bool)
parseField str =
  let lower = map toLower str
      checkField s =
        if (map toLower s) == lower
        then (s, True)
        else (s, False)
      flds = checkField <$> fieldNames
  in checkFields flds

-- | Checks the fields with the True value to ensure there is only one.
checkFields :: F.Fields (String, Bool) -> Ex.Exceptional Error (F.Fields Bool)
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
