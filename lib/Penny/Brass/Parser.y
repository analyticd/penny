{
module Penny.Brass.Parser where

import qualified Penny.Brass.AlexInput as A
import qualified Penny.Brass.Scanner as S
import qualified Penny.Brass.Start as T
import qualified Data.Text as X
import Penny.Lincoln.Strict (List((:|:), Empty))
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
         | Date { T.ItemDate $1 }

Comment : hash CommentContents newline MaybeSpaces { T.Comment $2 }

CommentContents : {- empty -} { Empty }
                | CommentContents CommentContent { $2 :|: $1 }

CommentContent
  : letters { T.CommentText $1 }
  | spaces { T.CommentText (T.spaces $1) }
  | digits { T.CommentText $1 }
  | exclamation { T.CommentText T.exclamation }
  | quote { T.CommentText T.quote }
  | hash { T.CommentText T.hash }
  | dollar { T.CommentText T.dollar }
  | percent { T.CommentText T.percent }
  | ampersand { T.CommentText T.ampersand }
  | apostrophe { T.CommentText T.apostrophe }
  | openParen { T.CommentText T.openParen }
  | closeParen { T.CommentText T.closeParen }
  | asterisk { T.CommentText T.asterisk }
  | plus { T.CommentText T.plus }
  | comma { T.CommentText T.comma }
  | dash { T.CommentText T.dash }
  | period { T.CommentText T.period }
  | slash { T.CommentText T.slash }
  | colon { T.CommentText T.colon }
  | semicolon { T.CommentText T.semicolon }
  | lessThan { T.CommentText T.lessThan }
  | equals   { T.CommentText T.equals }
  | greaterThan { T.CommentText T.greaterThan }
  | question { T.CommentText T.question }
  | atSign { T.CommentText T.atSign }
  | openBracket { T.CommentText T.openBracket }
  | backslash { T.CommentText T.backslash }
  | closeBracket { T.CommentText T.closeBracket }
  | caret { T.CommentText T.caret }
  | underscore { T.CommentText T.underscore }
  | backtick { T.CommentText T.backtick }
  | openBrace { T.CommentText T.openBrace }
  | verticalBar { T.CommentText T.verticalBar }
  | closeBrace { T.CommentText T.closeBrace }
  | tilde { T.CommentText T.tilde }
  | dr    { T.CommentText T.dr }
  | debit { T.CommentText T.debit }
  | cr     { T.CommentText T.cr }
  | credit { T.CommentText T.credit }

MaybeSpaces : {- empty -} { }
            | spaces { }

DateSeparator
  : dash { }
  | slash { }

Date
  : digits DateSeparator
    digits DateSeparator
    digits newline MaybeSpaces
    { T.Date $1 $3 $5 }
