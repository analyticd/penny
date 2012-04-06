{
module Penny.Brass.Parser where

import qualified Penny.Brass.AlexInput as A
import qualified Penny.Brass.Scanner as S
}

%name brass
%tokentype { A.Token }
%error { S.parseError }
%monad { S.StateM }
%lexer { S.lexer }

%token
    hash    { Hash }
    letters { UpperLowerOther $$ }
    spaces  { Spaces $$ }

%%

