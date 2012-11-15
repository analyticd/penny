module Penny.Copper.Render where

import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.List (intersperse, intercalate)
import Data.List.Split (splitEvery, splitOn)
import qualified Data.Text as X
import Data.Text (Text, cons, snoc)
import qualified Penny.Copper.Terminals as T
import qualified Data.Time as Time
import qualified Penny.Copper.Types as Y
import qualified Penny.Lincoln as L
import System.Locale (defaultTimeLocale)

-- | Is True if a sub account can be rendered at Level 1;
-- False otherwise.
isSubAccountLvl1 :: L.SubAccount -> Bool
isSubAccountLvl1 (L.SubAccount x) =
  X.all T.lvl1AcctChar x && not (X.null x)

isAccountLvl1 :: L.Account -> Bool
isAccountLvl1 (L.Account ls) =
  (not . null $ ls)
  && (all isSubAccountLvl1 ls)

lvl1Account :: L.Account -> Maybe Text
lvl1Account a@(L.Account ls) = do
  guard (isAccountLvl1 a)
  let txt = X.concat . intersperse (X.singleton ':')
            . map L.unSubAccount $ ls
  return $ '{' `X.cons` txt `X.snoc` '}'

isFirstSubAccountLvl2 :: L.SubAccount -> Bool
isFirstSubAccountLvl2 (L.SubAccount x) = case X.uncons x of
  Nothing -> False
  Just (c, r) -> T.letter c && (X.all T.lvl2AcctOtherChar r)

isOtherSubAccountLvl2 :: L.SubAccount -> Bool
isOtherSubAccountLvl2 (L.SubAccount x) =
  (not . X.null $ x)
  && (X.all T.lvl2AcctOtherChar x)

isAccountLvl2 :: L.Account -> Bool
isAccountLvl2 (L.Account ls) = case ls of
  [] -> False
  x:xs -> isFirstSubAccountLvl2 x && all isOtherSubAccountLvl2 xs

lvl2Account :: L.Account -> Maybe Text
lvl2Account a@(L.Account ls) = do
  guard $ isAccountLvl2 a
  return . X.concat . intersperse (X.singleton ':')
         . map L.unSubAccount $ ls

-- | Shows an account, with the minimum level of quoting
-- possible. Fails with an error if any one of the characters in the
-- account name does not satisfy the 'lvl1Char' predicate. Otherwise
-- returns a rendered account, quoted if necessary.
account :: L.Account -> Maybe Text
account a = lvl2Account a <|> lvl1Account a

-- | Specifies how to perform digit grouping when rendering a
-- quantity. All grouping groups into groups of 3 digits.
data GroupSpec =
  NoGrouping
  -- ^ Do not perform any digit grouping
  | GroupLarge
    -- ^ Group digits, but only if the number to be grouped is greater
    -- than 9,999 (if grouping the whole part) or if there are more
    -- than 4 decimal places (if grouping the fractional part).
  | GroupAll
    -- ^ Group digits whenever there are at least four decimal places.
  deriving (Eq, Show)


data GroupSpecs = GroupSpecs
  { left :: GroupSpec
  , right :: GroupSpec
  } deriving Show


grouper :: String
grouper = "\x2009"

radix :: String
radix = "."

-- | Performs grouping for amounts to the left of the radix point.
groupWhole :: GroupSpec -> String -> String
groupWhole gs o = let
  grouped = intercalate grouper
            . reverse
            . map reverse
            . splitEvery 3
            . reverse
            $ o
  in case gs of
    NoGrouping -> o
    GroupLarge -> if length o > 4 then grouped else o
    GroupAll -> grouped

-- | Performs grouping for amounts to the right of the radix point.
groupDecimal :: GroupSpec -> String -> String
groupDecimal gs o = let
  grouped = intercalate grouper
            . splitEvery 3
            $ o
  in case gs of
    NoGrouping -> o
    GroupLarge -> if length o > 4 then grouped else o
    GroupAll -> grouped

-- | Renders an unquoted Qty. Performs digit grouping as requested.
qty
  :: GroupSpecs
  -- ^ Group for the portion to the left and right of the radix point?

  -> L.Qty
  -> X.Text
qty gs q =
  let qs = show q
  in X.pack $ case splitOn "." qs of
    w:[] -> groupWhole (left gs) w
    w:d:[] ->
      groupWhole (left gs) w ++ radix ++ groupDecimal (right gs) d
    _ -> error "Qty.hs: rendering error"

-- | Render a quoted Level 1 commodity. Fails if any character does
-- not satisfy lvl1Char.
lvl1CmdtyQuoted :: L.Commodity -> Maybe Text
lvl1CmdtyQuoted (L.Commodity c) =
  if X.all T.lvl1CmdtyChar c
  then Just $ '"' `cons` c `snoc` '"'
  else Nothing


-- | Render a Level 2 commodity. Fails if the first character is not a
-- letter or a symbol, or if any other character is a space.
lvl2Cmdty :: L.Commodity -> Maybe Text
lvl2Cmdty (L.Commodity c) = do
  (f, rs) <- X.uncons c
  guard $ T.lvl2CmdtyFirstChar f
  guard . X.all T.lvl2CmdtyOtherChar $ rs
  return c


-- | Render a Level 3 commodity. Fails if any character is not a
-- letter or a symbol.
lvl3Cmdty :: L.Commodity -> Maybe Text
lvl3Cmdty (L.Commodity c) =
  if (not . X.null $ c) && (X.all T.lvl3CmdtyChar c)
  then return c
  else Nothing

-- | Render an Amount. The Format is required so that the commodity
-- can be displayed in the right place.
amount ::
  GroupSpecs
  -> L.Format
  -> L.Amount
  -> Maybe X.Text
amount gs f a = let
  (qt, c) = (L.qty a, L.commodity a)
  q = qty gs qt
  ws = case L.between f of
    L.SpaceBetween -> X.singleton ' '
    L.NoSpaceBetween -> X.empty
  mayLvl3 = lvl3Cmdty c
  mayLvl2 = lvl2Cmdty c
  in do
    quotedLvl1 <- lvl1CmdtyQuoted c
    let (l, r) = case L.side f of
          L.CommodityOnLeft -> case mayLvl3 of
            Nothing -> (quotedLvl1, q)
            Just l3 -> (l3, q)
          L.CommodityOnRight -> case mayLvl2 of
            Nothing -> (q, quotedLvl1)
            Just l2 -> (q, l2)
    return $ X.concat [l, ws, r]

comment :: X.Text -> X.Text
comment x = '#' `cons` x `snoc` '\n'

-- | Render a DateTime. If the DateTime is midnight, then the time and
-- time zone will not be printed. Otherwise, the time and time zone
-- will both be printed. The test for time zone equality depends only
-- upon the time zone's offset from UTC.
dateTime :: L.DateTime -> X.Text
dateTime dt = X.pack $ Time.formatTime defaultTimeLocale fmt zt
  where
    zt = L.toZonedTime dt
    fmtLong = "%F %T %z"
    fmtShort = "%F"
    isUTC = L.timeZone dt == L.noOffset
    isMidnight = (L.hours dt, L.minutes dt, L.seconds dt)
                 == L.midnight
    fmt = if isUTC && isMidnight
          then fmtShort
          else fmtLong

