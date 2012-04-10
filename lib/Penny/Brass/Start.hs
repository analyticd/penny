module Penny.Brass.Start where

import qualified Data.Text as X

import Penny.Lincoln.Strict (List, Might)
import qualified Penny.Lincoln.Strict as R
import qualified Penny.Brass.Scanner as S

data PennyFile = PennyFile !(List FileItem)
                 deriving (Eq, Show)

data FileItem = ItemComment !Comment
                | ItemTransaction !Transaction
                | ItemPrice !Price
                | ItemBlankLine
                deriving (Eq, Show)

data Comment = Comment !(List X.Text)
               deriving (Show, Eq)

data Number = Number !(List X.Text)
              deriving (Show, Eq)

data Flag = Flag !(List X.Text)
            deriving (Show, Eq)

data Payee = Payee !X.Text !(List X.Text)
             deriving (Eq, Show)

data Date = Date !X.Text !X.Text !X.Text
            deriving (Eq, Show)

data HoursMins = HoursMins !X.Text !X.Text
                 deriving (Eq, Show)

data Secs = Secs !X.Text
            deriving (Eq, Show)

data HoursMinsSecs = HoursMinsSecs !HoursMins !(Might Secs)
                     deriving (Eq, Show)

data TzSign = TzPlus | TzMinus
            deriving (Show, Eq)

data TimeZone = TimeZone !TzSign !X.Text
                deriving (Show, Eq)

data TimeAndOrZone = TimeMaybeZone !HoursMinsSecs !(Might TimeZone)
                   | ZoneOnly !TimeZone
                   deriving (Show, Eq)

data DateTime = DateTime !S.Location !Date !(Might TimeAndOrZone)
                deriving (Show, Eq)

data TopLine = TopLine !Memo !S.Location !DateTime !(Might Flag)
               !(Might Number) !(Might Payee)
               deriving (Show, Eq)

data Account = Account !SubAccount !(List SubAccount)
               deriving (Show, Eq)

data SubAccount = SubAccount !X.Text !(List X.Text)
                  deriving (Show, Eq)

data Tag = Tag !X.Text !(List X.Text)
           deriving (Show, Eq)

data Tags = Tags !(List Tag)
            deriving (Show, Eq)

data SubCommodity = SubCommodity !X.Text !(List X.Text)
                    deriving (Show, Eq)

data Commodity = Commodity !SubCommodity !(List SubCommodity)
                 deriving (Show, Eq)

data QtyItem =
  QtyDigits !X.Text
  | QtyPeriod
  | QtyComma
  | QtySpace
  deriving (Show, Eq)

data Qty = Qty !S.Location !QtyItem !(List QtyItem)
           deriving (Show, Eq)

data Amount = AmtCmdtyOnRight !Qty !(Might Int) !Commodity
              | AmtCmdtyOnLeft !Commodity !(Might Int) !Qty
              deriving (Eq, Show)

data MemoLine = MemoLine !X.Text !(List X.Text)
                deriving (Eq, Show)

data Memo = Memo !S.Location !(List MemoLine)
            deriving (Show, Eq)

data DrCr = Debit | Credit
          deriving (Show, Eq)

data Entry = Entry !DrCr !Amount
             deriving (Show, Eq)

data Posting = Posting !S.Location !(Might Flag) !(Might Number)
               !(Might Payee) !AccountTagsEntry !Memo
               deriving (Show, Eq)

data AccountTagsEntry =
  AccountTagsEntry !Account !Tags !(Might Entry)
  deriving (Show, Eq)

data Transaction = Transaction !TopLine !Posting !Posting
                   !(List Posting)
                   deriving (Show, Eq)

data Price = Price !S.Location !DateTime !Commodity !Amount
             deriving (Show, Eq)

-- End Data

-- Reversers
revPennyFile :: PennyFile -> PennyFile
revPennyFile (PennyFile l) =
  PennyFile . R.reverse . fmap revFileItem $ l

revFileItem :: FileItem -> FileItem
revFileItem f = case f of
  ItemComment c -> ItemComment (revComment c)
  ItemTransaction t -> ItemTransaction (revTransaction t)
  ItemPrice p -> ItemPrice (revPrice p)
  ItemBlankLine -> ItemBlankLine

revComment :: Comment -> Comment
revComment (Comment x) = Comment . R.reverse $ x

revTransaction :: Transaction -> Transaction
revTransaction (Transaction tl p1 p2 pr) =
  Transaction (revTopLine tl) (revPosting p1) (revPosting p2)
  (R.reverse . fmap revPosting $ pr)

-- DateTime does not need to be reversed

revPrice :: Price -> Price
revPrice (Price l dt c a) = Price l dt (revCommodity c)
                          (revAmount a)

revNumber :: Number -> Number
revNumber (Number x) = Number . R.reverse $ x

revFlag :: Flag -> Flag
revFlag (Flag x) = Flag . R.reverse $ x

revPayee :: Payee -> Payee
revPayee (Payee x1 xs) = Payee x1 (R.reverse xs)

revTopLine :: TopLine -> TopLine
revTopLine (TopLine m l dt fl nu pa) =
  TopLine (revMemo m) l dt (fmap revFlag fl) (fmap revNumber nu)
  (fmap revPayee pa)

revAccount :: Account -> Account
revAccount (Account s1 sr) =
  Account (revSubAccount s1) (fmap revSubAccount (R.reverse sr))

revSubAccount :: SubAccount -> SubAccount
revSubAccount (SubAccount s1 sr) = SubAccount s1 (R.reverse sr)

revTag :: Tag -> Tag
revTag (Tag t1 tr) = Tag t1 (R.reverse tr)

revTags :: Tags -> Tags
revTags (Tags ts) = Tags (fmap revTag (R.reverse ts))

revSubCommodity :: SubCommodity -> SubCommodity
revSubCommodity (SubCommodity s1 ss) =
  SubCommodity s1 (R.reverse ss)

revCommodity :: Commodity -> Commodity
revCommodity (Commodity c1 cs) =
  Commodity c1 (fmap revSubCommodity (R.reverse cs))

revQty :: Qty -> Qty
revQty (Qty l i is) = Qty l i (R.reverse is)

revAmount :: Amount -> Amount
revAmount (AmtCmdtyOnRight q i c) =
  AmtCmdtyOnRight (revQty q) i (revCommodity c)
revAmount (AmtCmdtyOnLeft c i q) =
  AmtCmdtyOnLeft (revCommodity c) i (revQty q)

revMemoLine :: MemoLine -> MemoLine
revMemoLine (MemoLine t1 ts) = MemoLine t1 (R.reverse ts)

revMemo :: Memo -> Memo
revMemo (Memo l ms) = Memo l (fmap revMemoLine . R.reverse $ ms)

revEntry :: Entry -> Entry
revEntry (Entry dc a) = Entry dc (revAmount a)

revPosting :: Posting -> Posting
revPosting (Posting l f n p ate m) =
  Posting l (fmap revFlag f) (fmap revNumber n)
  (fmap revPayee p) (revAccountTagsEntry ate) (revMemo m)

revAccountTagsEntry :: AccountTagsEntry -> AccountTagsEntry
revAccountTagsEntry (AccountTagsEntry a t e) =
  AccountTagsEntry (revAccount a) (revTags t) (fmap revEntry e)



--
-- Helpers
--
spaces :: Int -> X.Text
spaces i = X.replicate i (X.singleton ' ')

exclamation :: X.Text
exclamation = X.singleton '!'

quote :: X.Text
quote = X.singleton '"'

hash :: X.Text
hash = X.singleton '#'

dollar :: X.Text
dollar = X.singleton '$'

percent :: X.Text
percent = X.singleton '%'

ampersand :: X.Text
ampersand = X.singleton '&'

apostrophe :: X.Text
apostrophe = X.singleton '\''

openParen :: X.Text
openParen = X.singleton '('

closeParen :: X.Text
closeParen = X.singleton ')'

asterisk :: X.Text
asterisk = X.singleton '*'

plus :: X.Text
plus = X.singleton '+'

comma :: X.Text
comma = X.singleton ','

dash :: X.Text
dash = X.singleton '-'

period :: X.Text
period = X.singleton '.'

slash :: X.Text
slash = X.singleton '/'

colon :: X.Text
colon = X.singleton ':'

semicolon :: X.Text
semicolon = X.singleton ';'

lessThan :: X.Text
lessThan = X.singleton '<'

equals :: X.Text
equals = X.singleton '='

greaterThan :: X.Text
greaterThan = X.singleton '>'

question :: X.Text
question = X.singleton '?'

atSign :: X.Text
atSign = X.singleton '@'

openBracket :: X.Text
openBracket = X.singleton '['

backslash :: X.Text
backslash = X.singleton '/'

closeBracket :: X.Text
closeBracket = X.singleton ']'

caret :: X.Text
caret = X.singleton '^'

underscore :: X.Text
underscore = X.singleton '_'

backtick :: X.Text
backtick = X.singleton '`'

openBrace :: X.Text
openBrace = X.singleton '{'

verticalBar :: X.Text
verticalBar = X.singleton '|'

closeBrace :: X.Text
closeBrace = X.singleton '}'

tilde :: X.Text
tilde = X.singleton '~'

dr :: X.Text
dr = X.pack "Dr"

debit :: X.Text
debit = X.pack "Debit"

cr :: X.Text
cr = X.pack "Cr"

credit :: X.Text
credit = X.pack "Credit"
