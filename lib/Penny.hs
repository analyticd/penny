{- | Penny - double-entry accounting system

Penny is organized into a tree of modules, each with a name. Check out
the links for details on each component of Penny.

"Penny.Cabin" - Penny reports. Depends on Lincoln and Liberty.

"Penny.Copper" - the Penny parser. Depends on Lincoln.

"Penny.Denver" - Imports from John Wiegley's Ledger

"Penny.Liberty" - Penny command line parser helpers. Depends on
Lincoln and Copper.

"Penny.Lincoln" - the Penny core. Depends on no other Penny components.

"Penny.Shield" the Penny runtime environment

"Penny.Zinc" - the Penny command-line interface. Depends on Cabin,
Copper, Liberty, and Lincoln.

The dependencies are represented as a dot file in doc/dependencies.dot
in the Penny git repository.

-}
module Penny where
