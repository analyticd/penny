{-# LANGUAGE Trustworthy #-}
-- | Copper - the default Penny parser
--
-- Copper runs in two phases.  The first phase transforms a string
-- into an abstract syntax tree, or AST.  It uses uu-parsinglib to do
-- this.  The AST, which is in "Penny.Copper.Ast", is an entirely
-- context-free grammar.  Most error handling occurs within this
-- grammar itself--that is, the grammar is meticulously written so
-- that invalid constructs are not valid in the grammar.
--
-- The grammar does a remarkable amount of work: it ensures the
-- correctness of all dates (even leap days).  It constrains number
-- representations to permissible forms (even ensuring that grouping
-- constructs are valid--for instance, you cannot have two grouping
-- characters in a row.)  It ensures that if the user enters a side
-- (Debit, using @<@, or Credit, using @>@) that she does not enter a
-- number representation that is zero.
--
-- However, the grammar cannot do everything.  It cannot ensure that
-- transactions are balanced or that prices are constructed using
-- different commodities for the from-commodity and the to-commodity.
-- So, a valid AST is not necessarily a valid Copper file.  That's
-- where "Penny.Copper.ConvertAst" comes in.  It checks these
-- constructs for validity, while also performing some injective
-- transformations (for example, it transforms quoted strings to their
-- equivalent Text forms.)
--
-- This module contains 'copperParser', a single function that both
-- constructs the AST and transforms it to Lincoln types using
-- "Penny.Copper.ConvertAst".
--
-- The modules in uu-parsinglib are not Safe (in the Safe Haskell
-- sense) so this module is marked Trustworthy.
module Penny.Copper (copperParser) where

import Penny.Copper.Ast
import Penny.Copper.ConvertAst
import Penny.Lincoln
import Penny.Copper.Parser
import Text.ParserCombinators.UU.BasicInstances
import Data.List (intersperse)

-- | Constructs the Copper AST and transforms it to Lincoln types
-- using "Penny.Copper.ConvertAst".
--
-- Returns either an error or the results.  If there is an error,
-- the string is never empty; however, the type system does nothing
-- to ensure this.  The error string is a human-readable error
-- message.  For more flexibility (for example, to examine the error
-- types directly rather than getting a human-readable string) use
-- the functions and types in "Penny.Copper.Ast" and
-- "Penny.Copper.ConvertAst".
--
-- Also, this function is somewhat strict, as examining the result
-- will force the entire input string to be parsed.  As a practical
-- matter, that's typically what you would want anyway.  But for
-- more flexibility, again, consult "Penny.Copper.Ast" and
-- "Penny.Copper.ConvertAst", as the underlying parser and
-- conversion functions have the potential to be more lazy than this
-- function is.

copperParser
  :: String
  -> Either String [Either Price Transaction]
copperParser inp = do
  let (a, es1, es2) = parseAst inp
  ast <- case es1 ++ es2 of
    [] -> return a
    xs -> Left $ formatParseErrors xs
  case convertItemsFromAst ast of
    Left ers -> Left $ formatConvertErrors ers
    Right g -> return g

formatParseErrors :: [Error LineColPosA] -> String
formatParseErrors = unlines . intersperse "" . ("Parse error:":)
  . map displayParseError

formatConvertErrors :: (ConvertE, [ConvertE]) -> String
formatConvertErrors (c1, cs)
  = unlines . ("Error interpreting input file:":) . concat
  . intersperse [""] . (friendly c1 :) . map friendly $ cs
