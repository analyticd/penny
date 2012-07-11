-- | Zinc operands
--
-- This filter is used by other parts of Penny, too--the Postings
-- report also uses this module to filter out which postings should
-- appear in the report. So, this filter has only options that apply
-- to any PostingBox. Other options are inside other modules. For
-- instance the main filter specification also allows options like
-- --fwd-seq-unsorted; these are handled elsewhere.
--
-- All options that the filter takes do one of two things: either they
-- affect the matcher that is used to determine whether textual fields
-- are a match, or they add tokens that are later parsed by an
-- expression parser.
module Penny.Liberty.Operands where

