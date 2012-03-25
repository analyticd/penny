{
module Main where
}

%wrapper "posn-bytestring"

$digit      = 0-9
$upper      = A-Z
$lower      = a-z
$other      = \x80-\x10ffff
$space      = \x20
$newline    = \x0A
$dollar     = \$

tokens :-

$space+ { Spaces }
$newline { Newline }
\[      { OpenBracket }
\]      { CloseBracket }
\/      { Slash }
\-      { Dash }
\:      { Colon }
\*      { Asterisk }
\;      { Semicolon }
\<       { LessThan }
\>       { GreaterThan }
\'       { Apostrophe }
\"       { Quote }
\.      { Period }
\,      { Comma }
\#       { Hash }
\+      { Plus }
@       { AtSign }
\^      { Caret }
\$      { Dollar }

[ $upper $lower $other $dollar ]+         { LettersOthersDollars }
$digit+                                   { AllDigits }
$digit [ $upper $lower $other $dollar]*   { StartsWithDigit }
[ $upper $lower $other $dollar $digit ]+  { Word }

{

data Token =
  Spaces                                     AlexPosn ByteString.ByteString
  | Newline                                  AlexPosn ByteString.ByteString
  | OpenBracket                              AlexPosn ByteString.ByteString
  | CloseBracket                             AlexPosn ByteString.ByteString
  | Slash                                    AlexPosn ByteString.ByteString
  | Dash                                     AlexPosn ByteString.ByteString
  | Colon                                    AlexPosn ByteString.ByteString
  | Asterisk                                 AlexPosn ByteString.ByteString
  | Semicolon                                AlexPosn ByteString.ByteString
  | LessThan                                 AlexPosn ByteString.ByteString
  | GreaterThan                              AlexPosn ByteString.ByteString
  | Apostrophe                               AlexPosn ByteString.ByteString
  | Quote                                    AlexPosn ByteString.ByteString
  | Period                                   AlexPosn ByteString.ByteString
  | Comma                                    AlexPosn ByteString.ByteString
  | Hash                                     AlexPosn ByteString.ByteString
  | Plus                                     AlexPosn ByteString.ByteString
  | AtSign                                   AlexPosn ByteString.ByteString
  | Caret                                    AlexPosn ByteString.ByteString
  | Dollar                                   AlexPosn ByteString.ByteString
  | LettersOthersDollars                     AlexPosn ByteString.ByteString
  | AllDigits                                AlexPosn ByteString.ByteString
  | StartsWithDigit                          AlexPosn ByteString.ByteString
  | Word                                     AlexPosn ByteString.ByteString
  deriving Show

main = do
  c <- ByteString.getContents
  let tokens = alexScanTokens c
  putStr (show c)

}
