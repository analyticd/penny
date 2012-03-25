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

$notSpecial = [^\x01]
              

$notSpace = [^ \x20 \t \x0A ]

$special = [ $lowAscii $space $lowSymbol $digit $midSymbol
             $highSymbol $topSymbol $delete $newline ]

lex :-

$space+ ;
$newline { \p _ -> BlankLine p }
"#" .* \n { Comment }
\; .* \n { PostingMemo }
$notSpecial $notSpace* { Word }


{
data Token =
  BlankLine AlexPosn
  | Comment AlexPosn ByteString.ByteString
  | PostingMemo AlexPosn ByteString.ByteString
  | Word AlexPosn ByteString.ByteString

}
