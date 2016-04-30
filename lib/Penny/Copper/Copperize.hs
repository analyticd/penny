module Penny.Copper.Copperize where

import qualified Control.Lens as Lens
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X

import Penny.Copper.Types
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
import Penny.Copper.Optics


cZero :: Zero Char ()
cZero = Zero ('0', ())

cOne :: One Char ()
cOne = One ('1', ())

cTwo :: Two Char ()
cTwo = Two ('2', ())

cThree :: Three Char ()
cThree = Three ('3', ())

cFour :: Four Char ()
cFour = Four ('4', ())

cFive :: Five Char ()
cFive = Five ('5', ())

cSix :: Six Char ()
cSix = Six ('6', ())

cSeven :: Seven Char ()
cSeven = Seven ('7', ())

cEight :: Eight Char ()
cEight = Eight ('8', ())

cNine :: Nine Char ()
cNine = Nine ('9', ())

cThinSpace :: ThinSpace Char ()
cThinSpace = ThinSpace ('\x2009', ())

cUnderscore :: Underscore Char ()
cUnderscore = Underscore ('_', ())

cPeriod :: Period Char ()
cPeriod = Period ('.', ())

cComma :: Comma Char ()
cComma = Comma (',', ())

cRadixCom :: RadixCom Char ()
cRadixCom = RadixCom (',', ())

cRadixPer :: RadixPer Char ()
cRadixPer = RadixPer ('.', ())

cHyphen :: Hyphen Char ()
cHyphen = Hyphen ('-', ())

cSlash :: Slash Char ()
cSlash = Slash ('/', ())

cNewline :: Newline Char ()
cNewline = Newline ('\n', ())

cHash :: Hash Char ()
cHash = Hash ('#', ())

cColon :: Colon Char ()
cColon = Colon (':', ())

cPlus :: Plus Char ()
cPlus = Plus ('+', ())

cMinus :: Minus Char ()
cMinus = Minus ('-', ())

cBacktick :: Backtick Char ()
cBacktick = Backtick ('`', ())

cDoubleQuote :: DoubleQuote Char ()
cDoubleQuote = DoubleQuote ('"', ())

cBackslash :: Backslash Char ()
cBackslash = Backslash ('\\', ())

cSpace :: Space Char ()
cSpace = Space (' ', ())

cTab :: Tab Char ()
cTab = Tab ('\t', ())

cLessThan :: LessThan Char ()
cLessThan = LessThan ('<', ())

cGreaterThan :: GreaterThan Char ()
cGreaterThan = GreaterThan ('>', ())

cOpenSquare :: OpenSquare Char ()
cOpenSquare = OpenSquare (']', ())

cCloseSquare :: CloseSquare Char ()
cCloseSquare = CloseSquare (']', ())

cOpenCurly :: OpenCurly Char ()
cOpenCurly = OpenCurly ('{', ())

cCloseCurly :: CloseCurly Char ()
cCloseCurly = CloseCurly ('}', ())

cSemicolon :: Semicolon Char ()
cSemicolon = Semicolon (';', ())

cAtSign :: AtSign Char ()
cAtSign = AtSign ('@', ())

cCommentChar :: Char -> Maybe (CommentChar Char ())
cCommentChar c = Lens.preview _CommentChar (c, ())

-- | Converts a 'Text' to a 'CommentChar\'Star'.  Fails if any
-- character cannot be placed into a 'CommentChar' and returns a
-- 'Left' with the bad character.
cCommentChar'Star :: Text -> Either Char (CommentChar'Star Char ())
cCommentChar'Star
  = fmap CommentChar'Star . traverse f . Seq.fromList . X.unpack
  where
    f c = case cCommentChar c of
      Nothing -> Left c
      Just r -> Right r

cComment :: Text -> Either Char (Comment Char ())
cComment txt
  = Comment
  <$> pure cHash
  <*> cCommentChar'Star txt
  <*> pure cNewline

cWhite'Space :: White Char ()
cWhite'Space = White'Space cSpace

cWhite'Tab :: White Char ()
cWhite'Tab = White'Tab cTab

cWhite'Newline :: White Char ()
cWhite'Newline = White'Newline cNewline

cWhite'Comment :: Text -> Either Char (White Char ())
cWhite'Comment txt = White'Comment <$> cComment txt

-- | Replicates the given 'White' to return a 'White\'Star'.
cWhite'Star :: Int -> White Char () -> White'Star Char ()
cWhite'Star i w = White'Star (Seq.replicate i w)
