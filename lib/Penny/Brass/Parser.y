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
    hash    { A.Hash }
    letters { A.UpperLowerOther $$ }
    spaces  { A.Spaces $$ }
    digitsShort { A.DigitsShort $$ }
    digitsLong { A.DigitsLong $$ }
    newline { A.Newline }
    exclamation { A.Exclamation }

%%

Comment : hash CommentContents newline MaybeSpaces { T.Comment $2 }

CommentContents : {- empty -} { Empty }
                | CommentContents CommentContent { $2 :|: $1 }

CommentContent : letters { T.CommentText $1 }
               | spaces { T.CommentText (T.spaces $1) }
               | digitsShort { T.CommentText $1 }
               | digitsLong { T.CommentText $1 }
               | exclamation { T.CommentText T.exclamation }

MaybeSpaces : {- empty -} { }
            | spaces { }
