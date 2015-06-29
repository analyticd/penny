{-|

= The Penny style guide

= Naming conventions

With all the names that are in Penny, it helps to have some naming
conventions.

== Abbreviations

Abbreviations are good.  They keep code terse and free up space for
more nebulous things that might need longer names.  However, only use
abbreviations if they are in this global list.  It's OK to add things
to the global list.

[@Amt@] Amount.  An amount is a quantity and a commodity.

[@Cy@]  Commodity.  e.g. dollars, barrels of oil, diamonds, nuclear
waste, euro.

[@Dec@] Decimal.  All decimals are arbitrary precision and don't use
floating point arithmetic.

[@Pos@] Positive integers.

[@Qty@] Quantity.  Is like a Decimal but its sign is a Debit or
Credit, if it is not zero.

[@Uns@] Unsigned integers.


== Modules

All modules are in the \"Penny\" hierarchy.  Modules are used for a
few purposes.  First, they promote type safety by allowing the use of
abstract data types.  They also help organize the code by keeping
things that \"belong together\" in the same module and in the same
hierarchy.  Under some circumstances, modules are also used for
namespaces.

== Types and exported type synonyms

I try to keep all data type names and exported type synonyms unique
when used unqualified; this extends to all names throughout the
library.  This is primarily because Haddock is much more usable when
all names are unique, as you may have noticed whenever you use your
mouse to hover over something named \"ByteString\" or \"Text\" to see
whether it is strict or lazy.  Haddock does have options to show all
names qualified, but that comes with various disadvantages.  Overall,
the Haskell community seems to frown on qualified type names, and I'm
not going to swim upstream on this.


== Function names

Though I try to keep all data type names and type synonyms unique,
I'm less strict when it comes to function names.  Haddock is still
quite usable with conflicting function names, as the most important
thing in a Haddock is a type signature and that contains type names,
not function names.  If needed, it is acceptable to create a new
module just for namespace control if it will have numerous names that
conflict with other names in Penny or with commonly used names such
as 'Prelude' names.

== Typeclasses

If something is a member of a typeclass, don't provide redundant
non-overloaded functions.  For instance, if you have a @Monoid@,
don't also provide an @empty@ function.

== Record syntax

Generally avoid it.  It results in lots of exported names that don't
do you much good.  Sometimes it is necessary for types that have many
fields; in those cases, consider isolating the type in its own module
so it has its own namespace.

== Function naming convention

It helps to have some conventions for naming functions.  Some general
rules:

* Abbreviations are acceptable, but only if they appear in the
  abbreviations glossary.

* Long names are acceptable if necessary

Some guidelines below help to standardize function names.  In
Haskell, the type of a function says an enormous amount about the
function, but names also help.  Prefixes are used to sort functions
into categories.  The categories are based on what the function
conceptually does.

[@b@] Builders.  Functions that construct or build items from
particular components.  Specify the thing being built.  Typically
there is only one way to build a thing and so there is no need to
specify the components used to build.

@
bAmt :: Cy -> Qty -> Amt
@

[@d@] Deconstructors.  Conceptually these extract a piece of data
from a larger type.  Sometimes they are not in fact doing this; what
matters is whether they are conceptually doing this.  Typically name
these with the thing being extracted first, followed by what it's
being extracted from:

@
dQtyAmt :: Amt -> Qty
@

[@c@] Calculators.  These take one or more values and compute a new
value.  Other than the prefix, naming is flexible and should reflect
what's being calculated.

@
cIncreaseExpt :: Uns -> Dec -> Dec
@

[@t@] Transformers.  I would have called these \"converters\", but
there's a conflict for the @c@ prefix.  These functions perform
conversions from one type to another; typically they should be
injective or bijective.  Typically name these with the thing being
converted to, followed by the thing being converted from.

@
tUnsPos :: Pos -> Uns
@

== Indentation style, etc.

About the only conventions followed here are:

* Typically indent two spaces; and

* Try to keep lines 80 columns or less.

I don't care much about other nitpicky stuff like how to indent @do@
blocks or @where@ clauses or @let@ versus @where@.  If it looks good
to somebody, it's fine.  Part of reading Haskell is knowing how to
read different styles.

-}
module Penny.Docs.Style where
