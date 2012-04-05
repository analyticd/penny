{
{-# OPTIONS_GHC -w #-}
module Penny.Brass.Lexer where
}

%wrapper "posn-bytestring"

$digit      = 0-9
$upper      = A-Z
$lower      = a-z
$other      = \x80-\x10ffff
$space      = \x20
$newline    = \x0A
$dollar     = \$

$syms1      = \!-\/
$syms2      = \:-\@
$syms3      = \[-\`
$syms4      = \{-\~

tokens :-

$space+  { \p s -> Spaces p (fromIntegral (ByteString.length s)) }
$newline { \p _ -> Newline p }
\!       { \p _ -> Exclamation p }
\"       { \p _ -> Quote p }
\#       { \p _ -> Hash p }
\$       { \p _ -> Dollar p }
\%       { \p _ -> Percent p }
\&       { \p _ -> Ampersand p }
\'       { \p _ -> Apostrophe p }
\(       { \p _ -> OpenParen p }
\)       { \p _ -> CloseParen p }
\*       { \p _ -> Asterisk p }
\+       { \p _ -> Plus p }
\,       { \p _ -> Comma p }
\-       { \p _ -> Dash p }
\.       { \p _ -> Period p }
\/       { \p _ -> Slash p }
\:       { \p _ -> Colon p }
\;       { \p _ -> Semicolon p }
\<       { \p _ -> LessThan p }
\=       { \p _ -> Equals p }
\>       { \p _ -> GreaterThan p }
\?       { \p _ -> Question p }
@        { \p _ -> AtSign p }
\[       { \p _ -> OpenBracket p }
\\       { \p _ -> Backslash p }
\]       { \p _ -> CloseBracket p }
\^       { \p _ -> Caret p }
\_       { \p _ -> Underscore p }
\`       { \p _ -> Backtick p }
\{       { \p _ -> OpenBrace p }
\|       { \p _ -> VerticalBar p }
\}       { \p _ -> CloseBrace p }
\~       { \p _ -> Tilde p }

"Dr"                                      { \p _ -> Dr p }
"Debit"                                   { \p _ -> Debit p }
"Cr"                                      { \p _ -> Cr p }
"Credit"                                  { \p _ -> Credit p }
[ $upper $lower $other ]+                 { UpperLowerOther }
$digit{1,8}                               { DigitsShort }
$digit{9,}                                { DigitsLong }

{

data Token =
  Spaces                              AlexPosn !Int
  | Newline                           AlexPosn
  | Exclamation                       AlexPosn
  | Quote                             AlexPosn
  | Hash                              AlexPosn
  | Dollar                            AlexPosn
  | Percent                           AlexPosn
  | Ampersand                         AlexPosn
  | Apostrophe                        AlexPosn
  | OpenParen                         AlexPosn
  | CloseParen                        AlexPosn
  | Asterisk                          AlexPosn
  | Plus                              AlexPosn
  | Comma                             AlexPosn
  | Dash                              AlexPosn
  | Period                            AlexPosn
  | Slash                             AlexPosn
  | Colon                             AlexPosn
  | Semicolon                         AlexPosn
  | LessThan                          AlexPosn
  | Equals                            AlexPosn
  | GreaterThan                       AlexPosn
  | Question                          AlexPosn
  | AtSign                            AlexPosn
  | OpenBracket                       AlexPosn
  | Backslash                         AlexPosn
  | CloseBracket                      AlexPosn
  | Caret                             AlexPosn
  | Underscore                        AlexPosn
  | Backtick                          AlexPosn
  | OpenBrace                         AlexPosn
  | VerticalBar                       AlexPosn
  | CloseBrace                        AlexPosn
  | Tilde                             AlexPosn
  | Dr                                AlexPosn
  | Debit                             AlexPosn
  | Cr                                AlexPosn
  | Credit                            AlexPosn
  | UpperLowerOther                   AlexPosn !ByteString.ByteString
  | DigitsShort                       AlexPosn !ByteString.ByteString
  | DigitsLong                        AlexPosn !ByteString.ByteString
  deriving Show

}
