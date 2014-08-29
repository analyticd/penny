module Penny.Numbers.Abstract.Grouping where


-- Groups digits.  Rules for digit grouping:
--
-- * Digits to the left of the radix are grouped only if there are at
-- least five digits to the left of the radix.  Then, they are grouped
-- into groups of three digits each.
--
-- * Digits to the right of the radix are never grouped.
--
-- As a corollary, zero values are never grouped, as they never have
-- more than one digit to the left of the radix point.
