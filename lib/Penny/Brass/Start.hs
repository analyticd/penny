module Penny.Brass.Start where

import qualified Data.Text as X

import Penny.Lincoln.Strict (List)

data FileItem = ItemComment Comment
                | ItemDate Date
                deriving Show

data Comment = Comment !(List CommentContent)
               deriving (Show, Eq)

data CommentContent = CommentText !X.Text
                      deriving (Show, Eq)

data Date = Date !X.Text !X.Text !X.Text
            deriving Show

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
