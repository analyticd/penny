module Penny.Copper.Render where

import Control.Monad (guard)
import Control.Applicative ((<$>), (<|>), (<*>))
import Data.Foldable (toList)
import Data.List (intersperse, intercalate)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Text as X
import Data.Text (Text, cons, snoc)
import qualified Penny.Copper.Terminals as T
import qualified Penny.Lincoln.Transaction as LT
import qualified Data.Time as Time
import qualified Penny.Copper.Types as Y
import qualified Penny.Lincoln as L
import Penny.Lincoln.Family.Family (parent)
import Penny.Lincoln.Family (orphans)
import qualified Data.Traversable as Tr

-- * Helpers

-- | Merges a list of words into one Text; however, if any given Text
-- is empty, that Text is first dropped from the list.
txtWords :: [X.Text] -> X.Text
txtWords xs = case filter (not . X.null) xs of
  [] -> X.empty
  rs -> X.unwords rs

-- | Takes a field that may or may not be present and a function that
-- renders it. If the field is not present at all, returns an empty
-- Text. Otherwise will succeed or fail depending upon whether the
-- rendering function succeeds or fails.
renMaybe :: Maybe a -> (a -> Maybe X.Text) -> Maybe X.Text
renMaybe mx f = case mx of
  Nothing -> Just X.empty
  Just a -> f a


-- * Accounts

-- | Is True if a sub account can be rendered at Level 1;
-- False otherwise.
isSubAcctLvl1 :: L.SubAccount -> Bool
isSubAcctLvl1 (L.SubAccount x) =
  X.all T.lvl1AcctChar x && not (X.null x)

isAcctLvl1 :: L.Account -> Bool
isAcctLvl1 (L.Account ls) =
  (not . null $ ls)
  && (all isSubAcctLvl1 ls)

quotedLvl1Acct :: L.Account -> Maybe Text
quotedLvl1Acct a@(L.Account ls) = do
  guard (isAcctLvl1 a)
  let txt = X.concat . intersperse (X.singleton ':')
            . map L.unSubAccount $ ls
  return $ '{' `X.cons` txt `X.snoc` '}'

isFirstSubAcctLvl2 :: L.SubAccount -> Bool
isFirstSubAcctLvl2 (L.SubAccount x) = case X.uncons x of
  Nothing -> False
  Just (c, r) -> T.letter c && (X.all T.lvl2AcctOtherChar r)

isOtherSubAcctLvl2 :: L.SubAccount -> Bool
isOtherSubAcctLvl2 (L.SubAccount x) =
  (not . X.null $ x)
  && (X.all T.lvl2AcctOtherChar x)

isAcctLvl2 :: L.Account -> Bool
isAcctLvl2 (L.Account ls) = case ls of
  [] -> False
  x:xs -> isFirstSubAcctLvl2 x && all isOtherSubAcctLvl2 xs

lvl2Acct :: L.Account -> Maybe Text
lvl2Acct a@(L.Account ls) = do
  guard $ isAcctLvl2 a
  return . X.concat . intersperse (X.singleton ':')
         . map L.unSubAccount $ ls

-- | Shows an account, with the minimum level of quoting
-- possible. Fails with an error if any one of the characters in the
-- account name does not satisfy the 'lvl1Char' predicate. Otherwise
-- returns a rendered account, quoted if necessary.
ledgerAcct :: L.Account -> Maybe Text
ledgerAcct a = lvl2Acct a <|> quotedLvl1Acct a

-- * Commodities

-- | Render a quoted Level 1 commodity. Fails if any character does
-- not satisfy lvl1Char.
quotedLvl1Cmdty :: L.Commodity -> Maybe Text
quotedLvl1Cmdty (L.Commodity c) =
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


-- * Quantities

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
            . chunksOf 3
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
            . chunksOf 3
            $ o
  in case gs of
    NoGrouping -> o
    GroupLarge -> if length o > 4 then grouped else o
    GroupAll -> grouped

-- | Renders an unquoted Qty. Performs digit grouping as requested.
quantity
  :: GroupSpecs
  -- ^ Group for the portion to the left and right of the radix point?

  -> L.Qty
  -> X.Text
quantity gs q =
  let qs = show q
  in X.pack $ case splitOn "." qs of
    w:[] -> groupWhole (left gs) w
    w:d:[] ->
      groupWhole (left gs) w ++ radix ++ groupDecimal (right gs) d
    _ -> error "Qty.hs: rendering error"

-- * Amounts

-- | Render an Amount. The Format is required so that the commodity
-- can be displayed in the right place.
amount ::
  GroupSpecs
  -> L.Format
  -> L.Amount
  -> Maybe X.Text
amount gs f a = let
  (qt, c) = (L.qty a, L.commodity a)
  q = quantity gs qt
  ws = case L.between f of
    L.SpaceBetween -> X.singleton ' '
    L.NoSpaceBetween -> X.empty
  mayLvl3 = lvl3Cmdty c
  mayLvl2 = lvl2Cmdty c
  in do
    quotedLvl1 <- quotedLvl1Cmdty c
    let (l, r) = case L.side f of
          L.CommodityOnLeft -> case mayLvl3 of
            Nothing -> (quotedLvl1, q)
            Just l3 -> (l3, q)
          L.CommodityOnRight -> case mayLvl2 of
            Nothing -> (q, quotedLvl1)
            Just l2 -> (q, l2)
    return $ X.concat [l, ws, r]

-- * Comments

comment :: Y.Comment -> Maybe X.Text
comment (Y.Comment x) =
  if (not . X.all T.nonNewline $ x)
  then Nothing
  else Just $ '#' `cons` x `snoc` '\n'

-- * DateTime

-- | Render a DateTime. The day is always printed. If the time zone
-- offset is not zero, then the time and time zone offset are both
-- printed. If the time zone offset is zero, then the hours and
-- minutes are printed, but only if the time is not midnight. If the
-- seconds are not zero, they are also printed.

dateTime :: L.DateTime -> X.Text
dateTime (L.DateTime d h m s z) = X.append xd xr
  where
    (iYr, iMo, iDy) = Time.toGregorian d
    xr = hoursMinsSecsZone h m s z
    dash = X.singleton '-'
    xd = X.concat [ showX iYr, dash, pad2 . showX $ iMo, dash,
                    pad2 . showX $ iDy ]

pad2 :: X.Text -> X.Text
pad2 = X.justifyRight 2 '0'

pad4 :: X.Text -> X.Text
pad4 = X.justifyRight 4 '0'

showX :: Show a => a -> X.Text
showX = X.pack . show

hoursMinsSecsZone
  :: L.Hours -> L.Minutes -> L.Seconds -> L.TimeZoneOffset -> X.Text
hoursMinsSecsZone h m s z =
  if z == L.noOffset && (h, m, s) == L.midnight
  then X.empty
  else let xhms = X.concat [xh, colon, xm, xs]
           xh = pad2 . showX . L.unHours $ h
           xm = pad2 . showX . L.unMinutes $ m
           xs = let secs = L.unSeconds s
                in if secs == 0
                   then X.empty
                   else ':' `X.cons` (pad2 . showX $ secs)
           off = L.offsetToMins z
           sign = X.singleton $ if off < 0 then '-' else '+'
           padded = pad4 . showX . abs $ off
           xz = if off == 0
                then X.empty
                else ' ' `X.cons` sign `X.append` padded
           colon = X.singleton ':'
       in ' ' `X.cons` xhms `X.append` xz

-- * Entries

entry
  :: GroupSpecs
  -> L.Format
  -> L.Entry
  -> Maybe X.Text
entry gs f (L.Entry dc a) = do
  amt <- amount gs f a
  let dcTxt = X.pack $ case dc of
        L.Debit -> "<"
        L.Credit -> ">"
  return $ X.append (X.snoc dcTxt ' ') amt

-- * Flags

flag :: L.Flag -> Maybe X.Text
flag (L.Flag fl) =
  if X.all T.flagChar fl
  then Just $ '[' `cons` fl `snoc` ']'
  else Nothing

-- * Memos

postingMemoLine :: X.Text -> Maybe X.Text
postingMemoLine x =
  if X.all T.nonNewline x
  then Just $ (X.replicate 8 (X.singleton ' '))
              `X.append` ('\'' `cons` x `snoc` '\n')
  else Nothing

postingMemo :: L.Memo -> Maybe X.Text
postingMemo (L.Memo ls) =
  if null ls
  then Nothing
  else fmap X.concat . mapM postingMemoLine $ ls

transactionMemoLine :: X.Text -> Maybe X.Text
transactionMemoLine x =
  if X.all T.nonNewline x
  then Just $ ';' `cons` x `snoc` '\n'
  else Nothing

transactionMemo :: L.Memo -> Maybe X.Text
transactionMemo (L.Memo ls) =
  if null ls
  then Nothing
  else fmap X.concat . mapM transactionMemoLine $ ls

-- * Numbers

number :: L.Number -> Maybe Text
number (L.Number t) =
  if X.all T.numberChar t
  then Just $ '(' `cons` t `snoc` ')'
  else Nothing

-- * Payees

quotedLvl1Payee :: L.Payee -> Maybe Text
quotedLvl1Payee (L.Payee p) = do
  guard (X.all T.quotedPayeeChar p)
  return $ '~' `X.cons` p `X.snoc` '~'

lvl2Payee :: L.Payee -> Maybe Text
lvl2Payee (L.Payee p) = do
  (c1, cs) <- X.uncons p
  guard (T.letter c1)
  guard (X.all T.nonNewline cs)
  return p

payee :: L.Payee -> Maybe Text
payee p = lvl2Payee p <|> quotedLvl1Payee p

-- * Prices

price ::
  GroupSpecs
  -> L.PricePoint
  -> Maybe X.Text
price gs pp = let
  dateTxt = dateTime (L.dateTime pp)
  (L.From from) = L.from . L.price $ pp
  (L.To to) = L.to . L.price $ pp
  (L.CountPerUnit q) = L.countPerUnit . L.price $ pp
  mayFromTxt = lvl3Cmdty from <|> quotedLvl1Cmdty from
  amt = L.Amount q to
  in do
    fmt <- L.priceFormat . L.ppMeta $ pp
    let mayAmtTxt = amount gs fmt amt
    amtTxt <- mayAmtTxt
    fromTxt <- mayFromTxt
    return $
       (X.intercalate (X.singleton ' ')
       [X.singleton '@', dateTxt, fromTxt, amtTxt])
       `snoc` '\n'

-- * Tags

tag :: L.Tag -> Maybe X.Text
tag (L.Tag t) =
  if X.all T.tagChar t
  then Just $ X.cons '*' t
  else Nothing

tags :: L.Tags -> Maybe X.Text
tags (L.Tags ts) =
  X.intercalate (X.singleton ' ')
  <$> mapM tag ts

-- * TopLine

topLine :: LT.TopLine -> Maybe X.Text
topLine tl =
  f
  <$> renMaybe (LT.tMemo tl) transactionMemo
  <*> renMaybe (LT.tFlag tl) flag
  <*> renMaybe (LT.tNumber tl) number
  <*> renMaybe (LT.tPayee tl) payee
  where
    f meX flX nuX paX =
      meX
      `X.append` (txtWords [dtX, flX, nuX, paX])
      `X.snoc` '\n'
    dtX = dateTime (LT.tDateTime tl)

-- * Posting

-- | Renders a Posting. Fails if any of the components
-- fail to render. In addition, if the unverified Posting has an
-- Entry, a Format must be provided, otherwise render fails.
--
-- The columns look like this. Column numbers begin with 0 (like they
-- do in Emacs) rather than with column 1 (like they do in
-- Vim). (Really Emacs is the strange one; most CLI utilities seem to
-- start with column 1 too...)
--
-- > ID COLUMN WIDTH WHAT
-- > ---------------------------------------------------
-- > A    0      4     Blank spaces for indentation
-- > B    4      50    Flag, Number, Payee, Account, Tags
-- > C    54     2     Blank spaces for padding
-- > D    56     NA    Entry
--
-- Omit the padding after column B if there is no entry; also omit
-- columns C and D entirely if there is no Entry. (It is annoying to
-- have extraneous blank space in a file).
posting ::
  GroupSpecs
  -> L.Posting
  -> Maybe X.Text
posting gs p = do
  fl <- renMaybe (LT.pFlag p) flag
  nu <- renMaybe (LT.pNumber p) number
  pa <- renMaybe (LT.pPayee p) quotedLvl1Payee
  ac <- ledgerAcct (LT.pAccount p)
  ta <- tags (LT.pTags p)
  me <- renMaybe (LT.pMemo p) postingMemo
  maybePair <- case (LT.pInferred p, L.postingFormat . LT.pMeta $ p) of
    (LT.Inferred, _) -> return Nothing
    (LT.NotInferred, Just f) -> return (Just (LT.pEntry p, f))
    _ -> Nothing
  let renderEn (e, f) = entry gs f e
  en <- renMaybe maybePair renderEn
  return $ formatter fl nu pa ac ta en me

formatter ::
  X.Text    -- ^ Flag
  -> X.Text -- ^ Number
  -> X.Text -- ^ Payee
  -> X.Text -- ^ Account
  -> X.Text -- ^ Tags
  -> X.Text -- ^ Entry
  -> X.Text -- ^ Memo
  -> X.Text
formatter fl nu pa ac ta en me = let
  colA = X.pack (replicate 4 ' ')
  colBnoPad = txtWords [fl, nu, pa, ac, ta]
  colD = en
  colB = if X.null en
         then colBnoPad
         else X.justifyLeft 50 ' ' colBnoPad
  colC = if X.null en
         then X.empty
         else X.pack (replicate 2 ' ')
  rtn = X.singleton '\n'
  in X.concat [colA, colB, colC, colD, rtn, me]


-- * Transaction

transaction ::
  GroupSpecs
  -> L.Transaction
  -> Maybe X.Text
transaction gs txn = do
  let txnFam = LT.unTransaction txn
  tlX <- topLine (parent txnFam)
  pstgsX <- Tr.traverse (posting gs) (orphans txnFam)
  return $ tlX `X.append` (X.concat (toList pstgsX))

-- * Item

item :: GroupSpecs -> Y.Item -> Maybe X.Text
item gs i = case i of
  Y.BlankLine -> Just . X.singleton $ '\n'
  Y.IComment x -> comment x
  Y.PricePoint pp -> price gs pp
  Y.Transaction t -> transaction gs t

-- * Ledger

ledger :: GroupSpecs -> Y.Ledger -> Maybe X.Text
ledger gs (Y.Ledger is) = fmap X.concat . mapM (item gs) $ is
