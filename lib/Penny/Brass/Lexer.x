{
{-# OPTIONS_GHC -w #-}
module Penny.Brass.Lexer where

import Penny.Brass.AlexInput
}

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

$space+  { spaces }
$newline { const Newline }
\!       { const Exclamation }
\"       { const Quote }
\#       { const Hash }
\$       { const Dollar }
\%       { const Percent }
\&       { const Ampersand }
\'       { const Apostrophe }
\(       { const OpenParen }
\)       { const CloseParen }
\*       { const Asterisk }
\+       { const Plus }
\,       { const Comma }
\-       { const Dash }
\.       { const Period }
\/       { const Slash }
\:       { const Colon }
\;       { const Semicolon }
\<       { const LessThan }
\=       { const Equals }
\>       { const GreaterThan }
\?       { const Question }
@        { const AtSign }
\[       { const OpenBracket }
\\       { const Backslash }
\]       { const CloseBracket }
\^       { const Caret }
\_       { const Underscore }
\`       { const Backtick }
\{       { const OpenBrace }
\|       { const VerticalBar }
\}       { const CloseBrace }
\~       { const Tilde }

"Dr"                                      { const Dr }
"Debit"                                   { const Debit }
"Cr"                                      { const Cr }
"Credit"                                  { const Credit }
[ $upper $lower $other ]+                 { upperLowerOther }
$digit{1,8}                               { digitsShort }
$digit{9,}                                { digitsLong }
