{
module Penny.Brass.Lexer where
}

%wrapper "posn-bytestring"

$lowAscii   = \x00-\x1f      -- unprintable stuff
$space      = \x20
$lowSymbol  = \x21-\x2f      -- exclamation point to solidus
$digit      = 0-9
$midSymbol  = \x3a-\x40      -- colon to commercial at
$highSymbol = \x5b-\x60      -- left square bracket to grave accent
$topSymbol  = \x7b-\x7e      -- left curly bracket to tilde
$delete     = \x7f
$newline    = \x0a


$special = [ $lowAscii $space $lowSymbol $midSymbol
             $digit $highSymbol $topSymbol $delete ]

@nonSpace = [ ^ \n \t $space ]

tokens :-

-- actions must have type :: AlexPosn -> ByteString.ByteString -> token

$space+    ;
$newline { \p _ -> BlankLine p }

\#.* { \p s -> Comment p s }
\;.* { \p s -> PostingMemo p s }
[ ^ $special ] @nonSpace* { \p s -> Word p s }

{
data Token =
  BlankLine AlexPosn
  | Comment AlexPosn ByteString.ByteString
  | PostingMemo AlexPosn ByteString.ByteString
  | Word AlexPosn ByteString.ByteString

}
