-- | Commodities in Copper take one of two forms:
--
-- * a single-character currency symbol, or
--
-- * a longer name, where the first character is a letter and each
-- subsequent character is not a space or newline.  This name cannot
-- be empty.
--
-- There is no provision for the quoting of commodities.

module Penny.Copper.Commodity where
