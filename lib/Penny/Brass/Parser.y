{
module Penny.Brass.Parser where

import qualified Penny.Brass.AlexInput as A
import qualified Penny.Brass.Scanner as S
import qualified Penny.Brass.Start as T
import qualified Data.Text as X
import Penny.Lincoln.Strict
  (List((:|:), Empty), Might(Here, Nope))

}

%name brass
%tokentype { A.Token }
%error { S.parseError }
%monad { S.StateM }
%lexer { S.lexer } { A.EOF }

%token
    exclamation { A.Exclamation }
    quote { A.Quote }
    hash { A.Hash }
    dollar { A.Dollar }
    percent { A.Percent }
    ampersand { A.Ampersand }
    apostrophe { A.Apostrophe }
    openParen { A.OpenParen }
    closeParen { A.CloseParen }
    asterisk { A.Asterisk }
    plus { A.Plus }
    comma { A.Comma }
    dash { A.Dash }
    period { A.Period }
    slash { A.Slash }
    colon { A.Colon }
    semicolon { A.Semicolon }
    lessThan { A.LessThan }
    equals { A.Equals }
    greaterThan { A.GreaterThan }
    question { A.Question }
    atSign { A.AtSign }
    openBracket { A.OpenBracket }
    backslash { A.Backslash }
    closeBracket { A.CloseBracket }
    caret { A.Caret }
    underscore { A.Underscore }
    backtick { A.Backtick }
    openBrace { A.OpenBrace }
    verticalBar { A.VerticalBar }
    closeBrace { A.CloseBrace }
    tilde { A.Tilde }
    dr    { A.Dr }
    debit { A.Debit }
    cr    { A.Cr }
    credit { A.Credit }
    spaces { A.Spaces $$ }
    letters { A.Letters $$ }
    digits { A.Digits $$ }
    newline { A.Newline }

%%

--
-- PennyFile
--
PennyFile :: { List T.FileItem }
PennyFile : MaybeSpaces FileItems { $2 }

--
-- File Items
--

FileItems :: { List T.FileItem }
FileItems : {- empty -} { Empty }
          | FileItems FileItem { $2 :|: $1 }

FileItem :: { T.FileItem }
FileItem : Comment { T.ItemComment $1 }
         | Transaction { T.ItemTransaction $1 }
         | Price { T.ItemPrice $1 }
         | newline MaybeSpaces { T.ItemBlankLine }

--
-- Location
--

Location :: { S.Location }
Location : {- empty -} {% S.prevLocation }

--
-- Spaces
--

MaybeSpaces :: { Might Int }
MaybeSpaces : {- empty -} { Nope }
            | spaces { Here $1 }

--
-- Comments
--

Comment :: { T.Comment }
Comment : hash CommentContents newline MaybeSpaces { T.Comment $2 }

CommentContents :: { List X.Text }
CommentContents : {- empty -} { Empty }
                | CommentContents CommentContent { $2 :|: $1 }

CommentContent :: { X.Text }
CommentContent
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

--
-- Memo content and Transaction Memos
--

MemoContent :: { X.Text }
MemoContent
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

MemoContentList :: { List X.Text }
MemoContentList
  : {- empty -} { Empty }
  | MemoContentList MemoContent { $2 :|: $1 }

TransactionMemoLine :: { T.MemoLine }
TransactionMemoLine
  : semicolon MemoContent MemoContentList newline MaybeSpaces
    { T.MemoLine $2 $3 }

TransactionMemoList :: { List T.MemoLine }
TransactionMemoList
  : {- empty -} { Empty }
  | TransactionMemoList TransactionMemoLine { $2 :|: $1 }

TransactionMemo :: { T.Memo }
TransactionMemo : Location TransactionMemoList { T.Memo $1 $2 }

--
-- Number
--

Number :: { T.Number }
  : openParen NumberContents closeParen MaybeSpaces
    { T.Number $2 }

NumberContents :: { List X.Text }
NumberContents : NumberContent { $1 :|: Empty }
               | NumberContents NumberContent { $2 :|: $1 }

NumberContent :: { X.Text }
NumberContent
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

MaybeNumber :: { Might T.Number }
MaybeNumber : {- empty -} { Nope }
            | Number { Here $1 }

--
-- Flag
--

Flag :: { T.Flag }
Flag
  : openBracket FlagContents closeBracket MaybeSpaces
    { T.Flag $2 }

FlagContents :: { List X.Text }
FlagContents : FlagContent { $1 :|: Empty }
             | FlagContents FlagContent { $2 :|: $1 }

FlagContent :: { X.Text }
FlagContent
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

MaybeFlag :: { Might T.Flag }
MaybeFlag : {- empty -} { Nope }
          | Flag { Here $1 }

--
-- Payee
--

MaybeQuotedPayee :: { Might T.Payee }
MaybeQuotedPayee : {- empty -} { Nope }
                 | QuotedPayee { Here $1 }

QuotedPayee :: { T.Payee }
QuotedPayee
  : lessThan
    QuotedPayeeContent QuotedPayeeRests
    greaterThan
    MaybeSpaces { T.Payee $2 $3 }

QuotedPayeeContent :: { X.Text }
QuotedPayeeContent
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

QuotedPayeeRests :: { List X.Text }
QuotedPayeeRests
  : {- empty -} { Empty }
  | QuotedPayeeRests QuotedPayeeContent { $2 :|: $1 }

UnquotedPayee :: { T.Payee }
UnquotedPayee
  : UnquotedPayeeLeader UnquotedPayeeRests { T.Payee $1 $2 }

UnquotedPayeeLeader :: { X.Text }
UnquotedPayeeLeader
  : letters { $1 }

UnquotedPayeeRests :: { List X.Text }
UnquotedPayeeRests
  : {- empty -} { Empty }
  | UnquotedPayeeRests UnquotedPayeeRest { $2 :|: $1 }

UnquotedPayeeRest :: { X.Text }
UnquotedPayeeRest
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

--
-- Date
--

DateSeparator :: { }
DateSeparator
  : dash {  }
  | slash { }

Date :: { T.Date }
Date
  : digits DateSeparator
    digits DateSeparator
    digits MaybeSpaces
    { T.Date $1 $3 $5 }

HoursMins :: { T.HoursMins }
HoursMins
  : digits colon digits { T.HoursMins $1 $3 }

Secs :: { T.Secs }
Secs
  : colon digits { T.Secs $2 }

MaybeSecs :: { Might T.Secs }
MaybeSecs : Secs { Here $1 }
          | {- empty -} { Nope }

HoursMinsSecs :: { T.HoursMinsSecs }
HoursMinsSecs : HoursMins MaybeSecs MaybeSpaces { T.HoursMinsSecs $1 $2 }

TimeZone :: { T.TimeZone }
TimeZone : plus digits MaybeSpaces { T.TimeZone T.TzPlus $2 }
         | dash digits MaybeSpaces { T.TimeZone T.TzMinus $2 }

MaybeTimeMaybeZone :: { Might T.TimeAndOrZone }
MaybeTimeMaybeZone : {- empty -} { Nope }
                   | TimeAndOrZone { Here $1 }

TimeAndOrZone :: { T.TimeAndOrZone }
TimeAndOrZone : HoursMinsSecs MaybeZone { T.TimeMaybeZone $1 $2 }
              | TimeZone       { T.ZoneOnly $1 }

MaybeZone :: { Might T.TimeZone }
MaybeZone : {- empty -} { Nope }
          | TimeZone { Here $1 }

DateTime :: { T.DateTime }
DateTime : Date MaybeTimeMaybeZone { T.DateTime $1 $2 }

MaybeTopLinePayee :: { Might T.Payee }
MaybeTopLinePayee : {- empty -} { Nope }
                  | QuotedPayee { Here $1 }
                  | UnquotedPayee { Here $1 }

TopLine :: { T.TopLine }
TopLine : TransactionMemo Location DateTime MaybeFlag
          MaybeNumber MaybeTopLinePayee
          newline MaybeSpaces
          { T.TopLine $1 $2 $3 $4 $5 $6 }

--
-- Account
--

L1AcctChunk :: { X.Text }
L1AcctChunk
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

L1SubAcct :: { T.SubAccount }
L1SubAcct : L1AcctChunk L1SubAcctRest { T.SubAccount $1 $2 }

L1SubAcctRest :: { List X.Text }
L1SubAcctRest : {- empty -} { Empty }
              | L1SubAcctRest L1AcctChunk { $2 :|: $1 }

L1Account :: { T.Account }
L1Account : openBrace L1SubAcct L1AcctRest closeBrace MaybeSpaces
         { T.Account $2 $3 }

L1AcctRest :: { List T.SubAccount }
L1AcctRest : {- empty -} { Empty }
           | colon L1AcctList { $2 }

L1AcctList :: { List T.SubAccount }
L1AcctList : L1SubAcct { $1 :|: Empty }
           | L1AcctList colon L1SubAcct { $3 :|: $1 }

--
-- Level 2 Account
--
L2AcctFirstChunk :: { X.Text }
L2AcctFirstChunk
  : letters { $1 }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }


L2AcctOtherChunk :: { X.Text }
L2AcctOtherChunk
  : letters { $1 }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

L2FirstSubAcct :: { T.SubAccount }
L2FirstSubAcct : L2AcctFirstChunk L2AcctChunkList { T.SubAccount $1 $2 }

L2AcctChunkList :: { List X.Text }
L2AcctChunkList : {- empty -} { Empty }
                | L2AcctChunkList L2AcctOtherChunk { $2 :|: $1 }

L2OtherSubAcct :: { T.SubAccount }
L2OtherSubAcct : L2AcctOtherChunk L2AcctChunkList { T.SubAccount $1 $2 }

L2Account :: { T.Account }
L2Account : L2FirstSubAcct L2RestSubAccts { T.Account $1 $2 }

L2RestSubAccts :: { List T.SubAccount }
L2RestSubAccts : {- empty -} { Empty }
               | colon L2AcctList { $2 }

L2AcctList :: { List T.SubAccount }
L2AcctList : L2OtherSubAcct { $1 :|: Empty }
           | L2AcctList colon L2OtherSubAcct { $3 :|: $1 }

--
-- Tags
--
TagContent :: { X.Text }
TagContent
  : letters { $1 }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | colon { T.colon }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

Tag :: { T.Tag }
Tag : TagContent TagContents { T.Tag $1 $2 }

TagContents :: { List X.Text }
TagContents : {- empty -} { Empty }
            | TagContents TagContent { $2 :|: $1 }

TagList :: { List T.Tag }
TagList : Tag { $1 :|: Empty }
        | TagList spaces Tag { $3 :|: $1 }

Tags :: { T.Tags }
Tags : {- empty -} { T.Tags Empty }
     | asterisk TagList asterisk MaybeSpaces { T.Tags $2 }

--
-- Commodities
--

-- Level 1 Commodities

L1CmdtyContent :: { X.Text }
L1CmdtyContent
  : letters { $1 }
  | spaces { (T.spaces $1) }
  | digits { $1 }
  | exclamation { T.exclamation }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

L1SubCmdty :: { T.SubCommodity }
L1SubCmdty : L1CmdtyContent L1SubCmdtyContentList
             { T.SubCommodity $1 $2 }

L1SubCmdtyContentList :: { List X.Text }
L1SubCmdtyContentList : {- empty -} { Empty }
                      | L1SubCmdtyContentList L1CmdtyContent
                        { $2 :|: $1 }

L1Cmdty :: { T.Commodity }
L1Cmdty : quote
          L1SubCmdty L1TrailingCmdtys
          quote
          { T.Commodity $2 $3 }

L1TrailingCmdtys :: { List T.SubCommodity }
L1TrailingCmdtys : {- empty -} { Empty }
                 | colon L1TrailingCmdtyList { $2 }

L1TrailingCmdtyList :: { List T.SubCommodity }
L1TrailingCmdtyList : L1SubCmdty { $1 :|: Empty }
                    | L1TrailingCmdtyList colon L1SubCmdty
                      { $3 :|: $1 }

-- Level 2 commodities

L2FirstCmdtyLeader :: { X.Text }
L2FirstCmdtyLeader
  : letters { $1 }
  | dollar { T.dollar }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

L2CmdtyContent :: { X.Text }
L2CmdtyContent
  : letters { $1 }
  | digits { $1 }
  | exclamation { T.exclamation }
  | quote { T.quote }
  | hash { T.hash }
  | dollar { T.dollar }
  | percent { T.percent }
  | ampersand { T.ampersand }
  | apostrophe { T.apostrophe }
  | openParen { T.openParen }
  | closeParen { T.closeParen }
  | asterisk { T.asterisk }
  | plus { T.plus }
  | comma { T.comma }
  | dash { T.dash }
  | period { T.period }
  | slash { T.slash }
  | semicolon { T.semicolon }
  | lessThan { T.lessThan }
  | equals   { T.equals }
  | greaterThan { T.greaterThan }
  | question { T.question }
  | atSign { T.atSign }
  | openBracket { T.openBracket }
  | backslash { T.backslash }
  | closeBracket { T.closeBracket }
  | caret { T.caret }
  | underscore { T.underscore }
  | backtick { T.backtick }
  | openBrace { T.openBrace }
  | verticalBar { T.verticalBar }
  | closeBrace { T.closeBrace }
  | tilde { T.tilde }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

L2FirstSubCmdty :: { T.SubCommodity }
L2FirstSubCmdty : L2FirstCmdtyLeader L2FirstCmdtyRest
                  { T.SubCommodity $1 $2 }

L2FirstCmdtyRest :: { List X.Text }
L2FirstCmdtyRest : {- empty -} { Empty }
                 | L2FirstCmdtyRest L2CmdtyContent { $2 :|: $1 }

L2OtherSubCmdty :: { T.SubCommodity }
L2OtherSubCmdty : L2CmdtyContent L2OtherCmdtyRest
                  { T.SubCommodity $1 $2 }

L2OtherCmdtyRest :: { List X.Text }
L2OtherCmdtyRest : {- empty -} { Empty }
                 | L2OtherCmdtyRest L2CmdtyContent { $2 :|: $1 }

L2Cmdty :: { T.Commodity }
L2Cmdty : L2FirstSubCmdty L2RestCmdtys { T.Commodity $1 $2 }

L2RestCmdtys :: { List T.SubCommodity }
L2RestCmdtys : {- empty -} { Empty }
             | colon L2RestCmdtyList { $2 }

L2RestCmdtyList :: { List T.SubCommodity }
L2RestCmdtyList
  : L2OtherSubCmdty { $1 :|: Empty }
  | L2RestCmdtyList colon L2OtherSubCmdty { $3 :|: $1 }

-- Level 3 commodities

L3SubCmdtyContent :: { X.Text }
L3SubCmdtyContent
  : letters { $1 }
  | dollar { T.dollar }
  | dr    { T.dr }
  | debit { T.debit }
  | cr     { T.cr }
  | credit { T.credit }

L3SubCmdty :: { T.SubCommodity }
L3SubCmdty : L3SubCmdtyContent L3SubCmdtyContentRest
             { T.SubCommodity $1 $2 }

L3SubCmdtyContentRest :: { List X.Text }
L3SubCmdtyContentRest
  : {- empty -} { Empty }
  | L3SubCmdtyContentRest L3SubCmdtyContent { $2 :|: $1 }

L3SubCmdtyList :: { List T.SubCommodity }
L3SubCmdtyList
  : L3SubCmdty { $1 :|: Empty }
  | L3SubCmdtyList colon L3SubCmdty { $3 :|: $1 }

L3SubCmdtyRest :: { List T.SubCommodity }
L3SubCmdtyRest : {- empty -} { Empty }
               | colon L3SubCmdtyList { $2 }

L3Cmdty :: { T.Commodity }
L3Cmdty : L3SubCmdty L3SubCmdtyRest { T.Commodity $1 $2 }

LeftSideCmdty :: { T.Commodity }
LeftSideCmdty : L1Cmdty { $1 }
              | L3Cmdty { $1 }

RightSideCmdty :: { T.Commodity }
RightSideCmdty : L1Cmdty { $1 }
               | L2Cmdty { $1 }

--
-- Quantities
--

-- A quantity is parsed simply as a sequence of digits,
-- periods, commas, and spaces. A subsequent stage will make
-- sense of this jumble.

UnquotedQty :: { T.Qty }
UnquotedQty : Location UnquotedQtyItem
              UnquotedQtyItemList { T.Qty $1 $2 $3 }

UnquotedQtyItem :: { T.QtyItem }
UnquotedQtyItem : digits { T.QtyDigits $1 }
                | period { T.QtyPeriod }
                | comma { T.QtyComma }

UnquotedQtyItemList :: { List T.QtyItem }
UnquotedQtyItemList
  : {- empty -} { Empty }
  | UnquotedQtyItemList UnquotedQtyItem { $2 :|: $1 }

QuotedQty :: { T.Qty }
QuotedQty : Location caret QuotedQtyItem QuotedQtyItemList caret
            { T.Qty $1 $3 $4 }

QuotedQtyItem :: { T.QtyItem }
QuotedQtyItem : digits { T.QtyDigits $1 }
              | period { T.QtyPeriod }
              | comma { T.QtyComma }
              | spaces { T.QtySpace }

QuotedQtyItemList :: { List T.QtyItem }
QuotedQtyItemList
  : {- empty -} { Empty }
  | QuotedQtyItemList QuotedQtyItem { $2 :|: $1 }

Qty :: { T.Qty }
Qty : UnquotedQty { $1 }
    | QuotedQty { $1 }


--
-- DrCr
--
DrCr :: { T.DrCr }
DrCr : Debit MaybeSpaces { $1 }
     | Credit MaybeSpaces { $1 }

Debit :: { T.DrCr }
Debit : dr { T.Debit }
      | debit { T.Debit }

Credit :: { T.DrCr }
Credit : cr { T.Credit }
       | credit { T.Credit }

--
-- Entry
--
MaybeEntry :: { Might T.Entry }
MaybeEntry : {- empty -} { Nope }
           | Entry MaybeSpaces { Here $1 }

Entry :: { T.Entry }
Entry : DrCr Amount { T.Entry $1 $2 }

--
-- Amounts
--

Amount :: { T.Amount }
Amount : AmtCmdtyOnRight { $1 }
       | AmtCmdtyOnLeft { $1 }

AmtCmdtyOnRight :: { T.Amount }
AmtCmdtyOnRight : Qty MaybeSpaces RightSideCmdty
                  { T.AmtCmdtyOnRight $1 $2 $3 }

AmtCmdtyOnLeft :: { T.Amount }
AmtCmdtyOnLeft : LeftSideCmdty MaybeSpaces Qty
                 { T.AmtCmdtyOnLeft $1 $2 $3 }

--
-- Posting Memos
--

PostingMemoLine :: { T.MemoLine }
PostingMemoLine
  : apostrophe 
    MemoContent MemoContentList
    newline
    MaybeSpaces
    { T.MemoLine $2 $3 }

PostingMemoList :: { List T.MemoLine }
PostingMemoList : {- empty -} { Empty }
                | PostingMemoList PostingMemoLine { $2 :|: $1 }

PostingMemo :: { T.Memo }
PostingMemo : Location PostingMemoList { T.Memo $1 $2 }

--
-- AccountTagsEntry
--
L1AccountTagsEntry :: { T.AccountTagsEntry }
L1AccountTagsEntry : L1Account Tags MaybeEntry newline MaybeSpaces
                     { T.AccountTagsEntry $1 $2 $3 }

L2AccountTagsEntry :: { T.AccountTagsEntry }
L2AccountTagsEntry
  : L2Account newline MaybeSpaces
    { T.AccountTagsEntry $1 (T.Tags Empty) Nope }
  | L2Account spaces Tags MaybeEntry newline MaybeSpaces
    { T.AccountTagsEntry $1 $3 $4 }

AccountTagsEntry :: { T.AccountTagsEntry }
AccountTagsEntry : L1AccountTagsEntry { $1 }
                 | L2AccountTagsEntry { $1 }


--
-- Posting
--
Posting :: { T.Posting }
Posting :
  Location MaybeFlag MaybeNumber MaybeQuotedPayee
  AccountTagsEntry PostingMemo
  { T.Posting $1 $2 $3 $4 $5 $6 }

--
-- Transaction
--
MorePostings :: { List T.Posting }
MorePostings : {- empty -} { Empty }
             | MorePostings Posting { $2 :|: $1 }

Transaction :: { T.Transaction }
Transaction : TopLine Posting Posting MorePostings
              { T.Transaction $1 $2 $3 $4 }

--
-- Prices
--
Price :: { T.Price }
Price : atSign MaybeSpaces DateTime FromCmdty
        spaces Amount newline MaybeSpaces
        { T.Price $3 $4 $6 }

FromCmdty :: { T.Commodity }
FromCmdty : L1Cmdty { $1 }
          | L2Cmdty { $1 }
