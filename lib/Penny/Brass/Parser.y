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
    digitsShort { A.DigitsShort $$ }
    digitsLong { A.DigitsLong $$ }
    newline { A.Newline }

%%

FileItems : {- empty -} { Empty }
          | FileItems FileItem { $2 :|: $1 }

FileItem : Comment { T.ItemComment $1 }

Comment : hash CommentContents newline MaybeSpaces { T.Comment $2 }

CommentContents : {- empty -} { Empty }
                | CommentContents CommentContent { $2 :|: $1 }

CommentContent
  : letters { T.CommentText $1 }
  | spaces { T.CommentText (T.spaces $1) }
  | digitsShort { T.CommentText $1 }
  | digitsLong { T.CommentText $1 }
  | exclamation { T.CommentText T.exclamation }

MaybeSpaces : {- empty -} { }
            | spaces { }
