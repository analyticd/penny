module Penny.Brass.Start where

import qualified Data.Text as X

import Penny.Lincoln.Strict (List, Might)
import qualified Penny.Brass.Scanner as S

data FileItem = ItemComment !Comment
                | ItemTransaction !Transaction
                | ItemPrice !Price
                | ItemBlankLine
                deriving Show

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

data DateTime = DateTime !Date !(Might TimeAndOrZone)
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

data Price = Price !DateTime !Commodity !Amount
             deriving (Show, Eq)

-- End Data

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
