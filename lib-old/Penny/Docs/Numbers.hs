{- |

= Numbers in Penny

Getting numbers and arithmetic right in Penny was much more
challenging than I anticipated.  This module has some notes about the
way numbers work in Penny.

First, Penny distinguishes between numbers that users enter and
numbers that are used for arithmetic.  Numbers entered by users are
called /representations/.  A representation often includes information
such as grouping digits.  It also may contain a radix point.
Typically in computing the radix point is represented by the full-stop
character, @.@, but in currencies the radix point is often represented
by a comma, @,@.  Representations remember the exact grouping
characters used, as well as the character used for the radix point.

You can't do arithmetic on representations.  You can, however, convert
them to a number that is used for arithmetic.  In Penny, numbers that
can be used for arithmetic are known as /concrete/ numbers.  Concrete
numbers don't know anything about grouping characters or what
character is used for the radix point.

== Concrete numbers

The "Penny.Natural" module has so-called \"natural\" numbers.  These
are integers that are greater than or equal to zero.  All Penny
natural numbers are unbounded; that is, there is no maximum size.
'Penny.Natural.Positive' is for numbers that must be greater than
zero.  'Penny.Natural.Unsigned' is for numbers that are greater than
or equal to zero.  There is also a typeclass, 'Penny.Natural.Natural',
that provides some operations.  "Penny.Natural" provides various sorts
of functions to perform arithmetic on natural numbers.

'Penny.NonZero.NonZero' is for integers that are not equal to zero.
They can be positive or negative.  The "Penny.NonZero" module has
functions for working on 'Penny.NonZero.NonZero' numbers.
"Penny.Natural" is lower in the import hierarchy than "Penny.NonZero",
so functions to convert 'Penny.NonZero.NonZero' to "Penny.Natural"
types are in "Penny.NonZero".

'Penny.Decimal.Decimal' is the most important concrete number type.
It represents decimal numbers by using an 'Integer' coefficient and a
'Penny.Natural.Unsigned' exponent.  'Penny.Qty.Qty' and
'Penny.Qty.Exch' are @newtype@ wrappers around 'Penny.Decimal.Decimal'
to provide for the most common uses of 'Penny.Decimal.Decimal' values.

== Representations

The most primitive representations are in "Penny.Digit", which is full
of types that represent just a single digit.  More complex types are
in "Penny.Rep"; there are a myriad of types here so that you can
distinguish, for example, zero from non-zero representations on the
type level.

-}

module Penny.Docs.Numbers where
