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

FileItems : {- empty -} { Empty }
          | FileItems FileItem { $2 :|: $1 }

FileItem : Comment { T.ItemComment $1 }
         | TopLine { T.ItemTopLine $1 }
         | newline { T.ItemBlankLine }

Comment : hash CommentContents newline MaybeSpaces { T.Comment $2 }

CommentContents : {- empty -} { Empty }
                | CommentContents CommentContent { $2 :|: $1 }

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

MaybeSpaces : {- empty -} { }
            | spaces { }

DateSeparator
  : dash { }
  | slash { }

Number
  : openParen NumberContents closeParen MaybeSpaces
    { T.Number $2 }

NumberContents : NumberContent { $1 :|: Empty }
               | NumberContents NumberContent { $2 :|: $1 }

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

Flag
  : openBracket FlagContents closeBracket MaybeSpaces
    { T.Flag $2 }

FlagContents : FlagContent { $1 :|: Empty }
             | FlagContents FlagContent { $2 :|: $1 }

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

QuotedPayee
  : lessThan
    QuotedPayeeContent QuotedPayeeRests
    greaterThan
    MaybeSpaces { T.Payee $2 $3 }

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

QuotedPayeeRests
  : {- empty -} { Empty }
  | QuotedPayeeRests QuotedPayeeContent { $2 :|: $1 }

UnquotedPayee
  : UnquotedPayeeLeader UnquotedPayeeRests { T.Payee $1 $2 }

UnquotedPayeeLeader
  : letters { $1 }

UnquotedPayeeRests
  : {- empty -} { Empty }
  | UnquotedPayeeRests UnquotedPayeeRest { $2 :|: $1 }

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

Date
  : digits DateSeparator
    digits DateSeparator
    digits MaybeSpaces
    { T.Date $1 $3 $5 }

HoursMins
  : digits colon digits { T.HoursMins $1 $3 }

Secs
  : colon digits { T.Secs $2 }

MaybeSecs : Secs { Here $1 }
          | {- empty -} { Nope }

HoursMinsSecs : HoursMins MaybeSecs MaybeSpaces { T.HoursMinsSecs $1 $2 }

TimeZone : plus digits MaybeSpaces { T.TimeZone T.TzPlus $2 }
         | dash digits MaybeSpaces { T.TimeZone T.TzMinus $2 }

MaybeTimeMaybeZone : {- empty -} { Nope }
                   | TimeAndOrZone { Here $1 }

TimeAndOrZone : HoursMinsSecs MaybeZone { T.TimeMaybeZone $1 $2 }
              | TimeZone       { T.ZoneOnly $1 }

MaybeZone : {- empty -} { Nope }
          | TimeZone { Here $1 }

DateTime : Date MaybeTimeMaybeZone { T.DateTime $1 $2 }

-- TopLine

MaybeFlag : {- empty -} { Nope }
          | Flag { Here $1 }

MaybeNumber : {- empty -} { Nope }
            | Number { Here $1 }

MaybeTopLinePayee : {- empty -} { Nope }
                  | QuotedPayee { Here $1 }
                  | UnquotedPayee { Here $1 }

TopLine : DateTime MaybeFlag
          MaybeNumber MaybeTopLinePayee
          newline MaybeSpaces
          { T.TopLine $1 $2 $3 $4 }
