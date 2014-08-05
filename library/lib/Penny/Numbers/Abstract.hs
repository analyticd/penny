-- | Abstract numbers reflect the numbers that a user enters through
-- the regular Penny user interface (that is, in a ledger file, or on
-- the command line.)  This is not as straightforward as it might at
-- first seem; take for example the number @123456.78@.  The user
-- might enter it just that way, or she might enter @123,456.78@ or
-- even @123.456,78@.  The types in this hierarchy of modules capture
-- these details, and many others.
--
-- Generally you cannot do arithmetic using these types; rather,
-- "Penny.Numbers.Concrete" or one of the modules that uses it is
-- necessary for arithmetic.  Currently two modules use
-- "Penny.Numbers.Concrete": "Penny.Numbers.Qty" and
-- "Penny.Numbers.Exchange".  To convert between abstract and concrete
-- types, there is "Penny.Numbers.Babel".

module Penny.Numbers.Abstract where
