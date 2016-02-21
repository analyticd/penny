{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Copper - the default Penny parser
--
-- Copper runs in three phases.  The first phase transforms a string
-- into an abstract syntax tree, or AST.  It uses the Earley library
-- do this.  The grammar, in "Penny.Copper.Types", is a context-free
-- grammar created in "Penny.Copper.Grammar".
-- Most error handling occurs within this
-- grammar itself--that is, the grammar is meticulously written so
-- that invalid constructs are not valid in the grammar.
--
-- The grammar does a remarkable amount of work: it ensures the
-- correctness of all dates (even leap days).  It constrains number
-- representations to permissible forms (even ensuring that grouping
-- constructs are valid--for instance, you cannot have two grouping
-- characters in a row.)  It ensures that if the user enters a side
-- (Debit, using @<@, or Credit, using @>@) that she does not enter
-- a number representation that is zero.
--
-- The second phase transforms this AST into the data types that are
-- used in the rest of Penny.  This is called the @converter@ and it
-- is in "Penny.Copper.Converter".
--
-- The third phase performs final error checking.  It ensures that
-- postings are valid, that transactions are balanced, and that
-- prices are valid.  This phase is called the @proofer@ and it is
-- in "Penny.Copper.Proofer".
--
-- This module contains the functions that most of the rest of the
-- library would ordinarily need.
module Penny.Copper where

import Control.Exception (Exception)
import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Typeable (Typeable)
import qualified Data.Validation as V
import qualified Pinchot
import qualified Text.Earley as Earley

import qualified Penny.Copper.Converter as Converter
import qualified Penny.Copper.Types as Types
import qualified Penny.Copper.Grammar as Grammar
import qualified Penny.Copper.Proofer as Proofer
import Penny.Ents
import Penny.NonEmpty
import Penny.Price
import Penny.Realm
import Penny.Scalar
import Penny.SeqUtil
import Penny.Tree

-- | Given an integer position in a text, obtain the line and
-- column.
textPosition
  :: Int
  -- ^ Integer position
  -> Text
  -> Converter.Pos
textPosition int
  = X.foldl' add (Converter.Pos 1 1)
  . X.take int
  where
    add (Converter.Pos lin col) c
      | c == '\n' = Converter.Pos (lin + 1) 1
      | c == '\t' = Converter.Pos lin (col + 8 - ((col - 1) `mod` 8))
      | otherwise = Converter.Pos lin (col + 1)

-- | The name of a file being parsed.  This is optional and can be
-- the empty 'Text'.
type Filename = Text

data ParseErrorInfo = ParseErrorInfo
  { _failurePosition :: Converter.Pos
  , _expected :: [String]
  } deriving (Typeable, Show)

Lens.makeLenses ''ParseErrorInfo

data ParseFailReason
  = LeftoverInput
  | AbortedParse
  deriving (Typeable, Show)

Lens.makePrisms ''ParseFailReason

data ParseError = ParseError
  { _parseFailReason :: ParseFailReason
  , _parseErrorInfo :: ParseErrorInfo
  , _parseErrorFilename :: Filename
  } deriving (Typeable, Show)

Lens.makeLenses ''ParseError

errorInfo
  :: Text
  -- ^ Input text
  -> Earley.Report String Text
  -> ParseErrorInfo
errorInfo inp (Earley.Report pos exp _)
  = ParseErrorInfo (textPosition pos inp) exp

-- | Given a Pinchot rule, parse a complete string in that language.
runParser
  :: (forall r. Earley.Grammar r (Earley.Prod r String Char Types.WholeFile))
  -> (Filename, Text)
  -> Either ParseError Types.WholeFile
runParser grammar (filename, txt) = case results of
  result:[]
    | X.null (Earley.unconsumed report) -> Right result
    | otherwise -> Left (ParseError LeftoverInput info filename)
  [] -> Left (ParseError AbortedParse info filename)
  _ -> error "runParser: grammar is ambiguous."
  where
    (results, report) = Earley.fullParses (Earley.parser grammar) txt
    info = errorInfo txt report

-- | Creates a 'Tree' holding the filename.
filenameTree :: Filename -> Tree
filenameTree name = Tree System (Just (SText "filename"))
  [ Tree System (Just (SText name)) [] ]

-- | Given the results of a parse, append a filename tree to each
-- transaction.
appendFilenameTrees
  :: Functor f
  => Filename
  -> f (Either a (Seq Tree, b))
  -> f (Either a (Seq Tree, b))
appendFilenameTrees filename = Lens.over setter f
  where
    setter = Lens.mapped . Lens._Right . Lens._1
    f sq = sq `Lens.snoc` (filenameTree filename)

newtype ParseConvertProofError
  = ParseConvertProofError (Either ParseError (NonEmpty Proofer.ProofFail))
  deriving (Typeable, Show)

instance Exception ParseConvertProofError

-- | Parses, converts, and proofs a single file.
--
-- Also, appends the filename tree to each transaction.
parseConvertProof
  :: (Filename, Text)
  -> Either ParseConvertProofError
            (Seq Price, Seq (Seq Tree, Balanced (Seq Tree)))
parseConvertProof (filename, txt) = do
  wholeFile <- either (Left . ParseConvertProofError . Left) Right
    $ runParser grammar (filename, txt)
  let parts = Converter.runConverter . Converter.c'WholeFile $ wholeFile
  items <- either (Left . ParseConvertProofError . Right)
    Right . Lens.view V._Either . Proofer.proofItems
    $ parts
  return . partitionEithers . appendFilenameTrees filename $ items
  where
    grammar = $(Pinchot.earleyGrammar "Types"
      (fmap Grammar.wholeFile Grammar.grammar) )

