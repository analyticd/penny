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
$newline+ { \p _ -> Newline p }
\[      { \p _ -> OpenBracket p }
\]      { \p _ -> CloseBracket p }
\{      { \p _ -> OpenBrace p }
\}      { \p _ -> CloseBrace p }
\(      { \p _ -> OpenParen p }
\)      { \p _ -> CloseParen p }
\/      { \p _ -> Slash p }
\-      { \p _ -> Dash p }
\:      { \p _ -> Colon p }
\*      { \p _ -> Asterisk p }
\;      { \p _ -> Semicolon p }
\<       { \p _ ->  LessThan p }
\>       { \p _ -> GreaterThan p }
\'       { \p _ -> Apostrophe p }
\"       { \p _ -> Quote p }
\.      { \p _ -> Period p }
\,      { \p _ -> Comma p }
\#       { \p _ -> Hash p }
\+      { \p _ -> Plus p }
@       { \p _ -> AtSign p }
\^      { \p _ -> Caret p }
\$      { \p _ -> Dollar p }

"Dr"                                      { \p _ -> Debit p }
"Debit"                                   { \p _ -> Debit p }
"Cr"                                      { \p _ -> Credit p }
"Credit"                                  { \p _ -> Credit p }
[ $upper $lower $other ]+                 { UpperLowerOther }
$digit{1,8}                               { DigitsShort }
$digit{9,}                                { DigitsLong }
[ $syms1 $syms2 $syms3 $syms4 ]           { OtherSymbol }

{

data Token =
  Spaces                                     AlexPosn Int
  | Newline                                  AlexPosn
  | OpenBracket                              AlexPosn
  | CloseBracket                             AlexPosn
  | OpenBrace                                AlexPosn
  | CloseBrace                               AlexPosn
  | OpenParen                                AlexPosn
  | CloseParen                               AlexPosn
  | Slash                                    AlexPosn
  | Dash                                     AlexPosn
  | Colon                                    AlexPosn
  | Asterisk                                 AlexPosn
  | Semicolon                                AlexPosn
  | LessThan                                 AlexPosn
  | GreaterThan                              AlexPosn
  | Apostrophe                               AlexPosn
  | Quote                                    AlexPosn
  | Period                                   AlexPosn
  | Comma                                    AlexPosn
  | Hash                                     AlexPosn
  | Plus                                     AlexPosn
  | AtSign                                   AlexPosn
  | Caret                                    AlexPosn
  | Dollar                                   AlexPosn
  | Debit                                    AlexPosn
  | Credit                                   AlexPosn
  | UpperLowerOther                          AlexPosn ByteString.ByteString
  | DigitsShort                              AlexPosn ByteString.ByteString
  | DigitsLong                               AlexPosn ByteString.ByteString
  | OtherSymbol                              AlexPosn ByteString.ByteString
  deriving Show

}
