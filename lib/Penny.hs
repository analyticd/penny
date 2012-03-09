{- | Penny - double-entry accounting system

Penny is organized into a tree of modules, each with a name. Check out
the links for details on each component of Penny.

"Penny.Cabin" - Penny reports. Depends on Lincoln and Liberty.

"Penny.Copper" - the Penny parser. Depends on Lincoln.

"Penny.Liberty" - Penny command line parser helpers. Depends on Lincoln and Copper.

"Penny.Lincoln" - the Penny core. Depends on no other Penny components.

"Penny.Zinc" - the Penny command-line interface. Depends on Cabin, Copper, Liberty, and Lincoln.

A crude drawing of these dependencies appears in the source code for this module (I don't know if Haddock would like it.)

-}

{-

Graph:
                     +----------Zinc
                     |           |
                     |           |
                     |    +----Cabin
                     |    |      |
                     |    |      |
                     |----+-> Liberty----+
                     |    |      |       |
                     |    |      |       |
                     |----+-> Copper     |
                     |    |      |       |
                     |    |      |       |
                     +----+-> Lincoln <--+

-}
module Penny where
