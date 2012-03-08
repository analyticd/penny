-- | Liberty - Penny command line parsing utilities
--
-- Both Cabin and Zinc share various functions that aid in parsing
-- command lines. For instance both the Postings report and the Zinc
-- postings filter use common command-line options. However, Zinc
-- already depends on Cabin. To avoid a cyclic dependency whereby
-- Cabin would also depend on Zinc, functions formerly in Zinc that
-- Cabin will also find useful are relocated here, to Liberty.

module Penny.Liberty where
