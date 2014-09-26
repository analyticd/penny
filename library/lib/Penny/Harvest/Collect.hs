-- | Collection processes the lines in each package, collecting them
-- into postings.  The most important function here is
-- 'Penny.Harvest.Collect.Machine.collectPackages', which takes a
-- "Penny.Harvest.Serialize.Packages" and returns a sequence of
-- results; each result returns any postings from the package, along
-- with any error messages.

module Penny.Harvest.Collect where
