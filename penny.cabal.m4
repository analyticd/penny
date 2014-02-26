-- The penny.cabal file is generated using m4, the penny.cabal.m4
-- file, and the versions.m4 file.  The Makefile will generate
-- penny.cabal for you.

Name: penny
Version: pv_penny
Cabal-version: >=1.8
Build-Type: Simple
License: BSD3
Copyright: 2012-2014 Omari Norman.
author: Omari Norman
maintainer: omari@smileystation.com
stability: Experimental
homepage: http://www.github.com/massysett/penny
bug-reports: omari@smileystation.com
Category: Console, Finance
License-File: LICENSE

synopsis: Extensible double-entry accounting system

description:
  Penny is a double-entry accounting system.  You keep your records in a
  plain-text file, and Penny gives you useful reports in your UNIX shell.
  .
  For more information, please see
  .
  <http://www.github.com/massysett/penny>

extra-source-files:
    install-docs
  , README.md
  , doc/*.dot
  , doc/*.hs
  , doc/examples/*.pny
  , doc/man/*.1
  , doc/man/*.7
  , versions.m4
  , Makefile
  , current-versions.txt
  , minimum-versions.txt
  , penny.cabal.m4

source-repository head
  type: git
  location: git://github.com/massysett/penny.git

Library

  Build-depends:
      base >= pv_base && < pv_base_max

    -- Do not try to put comments on same line as data; Cabal does
    -- not allow this.

    -- Package                  Version
    , bytestring                >= pv_bytestring
    , containers                >= pv_containers
    , old-locale                >= pv_old_locale
    , parsec                    >= pv_parsec
    , split                     >= pv_split
    , text                      >= pv_text
    , time                      >= pv_time
    , transformers              >= pv_transformers

    -- Omari packages
    -- Package                  Version
    , anonymous-sums            >= pv_anonymous_sums
    , matchers                  >= pv_matchers
    , multiarg                  >= pv_multiarg
    , ofx                       >= pv_ofx
    , prednote                  >= pv_prednote
    , rainbow                   >= pv_rainbow

    -- Other packages
    -- Package                  Version
    , action-permutations       == pv_action_permutations
    , cereal                    >= pv_cereal
    , either                    >= pv_either
    , pretty-show               >= pv_pretty_show
    , semigroups                >= pv_semigroups

  Exposed-modules:
      Penny
    , Penny.Brenner
    , Penny.Brenner.Clear
    , Penny.Brenner.Database
    , Penny.Brenner.Import
    , Penny.Brenner.Info
    , Penny.Brenner.Merge
    , Penny.Brenner.OFX
    , Penny.Brenner.Print
    , Penny.Brenner.Types
    , Penny.Brenner.Util
    , Penny.Cabin
    , Penny.Cabin.Balance
    , Penny.Cabin.Balance.Convert
    , Penny.Cabin.Balance.Convert.Chunker
    , Penny.Cabin.Balance.Convert.ChunkerPct
    , Penny.Cabin.Balance.Convert.Options
    , Penny.Cabin.Balance.Convert.Parser
    , Penny.Cabin.Balance.MultiCommodity
    , Penny.Cabin.Balance.MultiCommodity.Chunker
    , Penny.Cabin.Balance.MultiCommodity.Parser
    , Penny.Cabin.Balance.Util
    , Penny.Cabin.Interface
    , Penny.Cabin.Meta
    , Penny.Cabin.Options
    , Penny.Cabin.Parsers
    , Penny.Cabin.Posts
    , Penny.Cabin.Posts.Allocated
    , Penny.Cabin.Posts.BottomRows
    , Penny.Cabin.Posts.Fields
    , Penny.Cabin.Posts.Growers
    , Penny.Cabin.Posts.Chunk
    , Penny.Cabin.Posts.Meta
    , Penny.Cabin.Posts.Parser
    , Penny.Cabin.Posts.Spacers
    , Penny.Cabin.Posts.Types
    , Penny.Cabin.Row
    , Penny.Cabin.Scheme
    , Penny.Cabin.Scheme.Schemes
    , Penny.Cabin.TextFormat
    , Penny.Copper
    , Penny.Copper.Interface
    , Penny.Copper.Parsec
    , Penny.Copper.Render
    , Penny.Copper.Terminals
    , Penny.Denver
    , Penny.Denver.Diff
    , Penny.Denver.Reprint
    , Penny.Denver.Selloff
    , Penny.Denver.Reconcile
    , Penny.Liberty
    , Penny.Lincoln
    , Penny.Lincoln.Balance
    , Penny.Lincoln.Bits
    , Penny.Lincoln.Bits.DateTime
    , Penny.Lincoln.Bits.Open
    , Penny.Lincoln.Bits.Price
    , Penny.Lincoln.Bits.Qty
    , Penny.Lincoln.Builders
    , Penny.Lincoln.Ents
    , Penny.Lincoln.Equivalent
    , Penny.Lincoln.HasText
    , Penny.Lincoln.Matchers
    , Penny.Lincoln.Natural
    , Penny.Lincoln.Predicates
    , Penny.Lincoln.Predicates.Siblings
    , Penny.Lincoln.PriceDb
    , Penny.Lincoln.Queries
    , Penny.Lincoln.Queries.Siblings
    , Penny.Lincoln.Serial
    , Penny.Shield
    , Penny.Steel
    , Penny.Wheat
    , Penny.Zinc

  Other-modules:
      Paths_penny

  hs-source-dirs: lib

  if flag(incabal)
    cpp-options: -Dincabal

  ghc-options: -Wall
  if flag(debug)
    ghc-options: -auto-all -caf-all

Test-Suite penny-test
  type: exitcode-stdio-1.0
  Main-is: penny-test.hs
  other-modules:
      Copper
    , Copper.Gen.Parsers
    , Copper.Gen.Terminals
    , Copper.Parser
    , Copper.Render
    , Lincoln
  hs-source-dirs: tests

  -- For details on why penny is a dependency here, see
  -- http://stackoverflow.com/questions/6711151

  build-depends:
      penny           == pv_penny
    , base            >= pv_base && < pv_base_max

    -- Packages I maintain
    , multiarg        >= pv_multiarg
    , anonymous-sums  >= pv_anonymous_sums

    -- Other packages
    , QuickCheck      >= pv_QuickCheck
    , random-shuffle  >= pv_random_shuffle
    , parsec          >= pv_parsec
    , semigroups      >= pv_semigroups
    , text            >= pv_text
    , time            >= pv_time
    , transformers    >= pv_transformers

  ghc-options: -Wall

Executable penny-gibberish
  Main-is: penny-gibberish.hs
  other-modules:
      Copper.Gen.Parsers
    , Copper.Gen.Terminals
  hs-source-dirs: tests

  if flag(build-gibberish)
    build-depends:
          penny           == pv_penny
        , base            >= pv_base && < pv_base_max

        -- Packages I maintain
        , multiarg        >= pv_multiarg

        -- Other packages
        , QuickCheck      >= pv_QuickCheck
        , random-shuffle  >= pv_random_shuffle
        , random          >= pv_random
        , semigroups      >= pv_semigroups
        , text            >= pv_text
        , time            >= pv_time
        , transformers    >= pv_transformers

  else
    buildable: False

  ghc-options: -Wall

Flag build-gibberish
  Description: Build the penny-gibberish executable
  Default: False

Executable penny
  Build-depends:
      penny ==pv_penny
    , base >= pv_base && < pv_base_max

  hs-source-dirs: bin
  Main-is: penny-main.hs
  Other-modules: Paths_penny
  GHC-Options: -Wall
  if flag(debug)
    GHC-Options: -rtsopts -auto-all -caf-all

  if ! flag(build-penny)
    buildable: False

Flag build-penny
  Description: Build the penny executable
  Default: True

Executable penny-selloff
  Build-depends:
      penny == pv_penny
    , base >= pv_base && < pv_base_max

  other-modules: Paths_penny
  hs-source-dirs: bin
  Main-is: penny-selloff.hs
  GHC-Options: -Wall
  if flag(debug)
    GHC-Options: -rtsopts -auto-all -caf-all

  if ! flag(build-selloff)
    buildable: False

Flag build-selloff
  Description: Build the penny-selloff executable
  Default: True

Executable penny-diff
  Build-depends:
      penny == pv_penny
    , base >= pv_base && < pv_base_max

  hs-source-dirs: bin
  Main-is: penny-diff.hs
  Other-modules: Paths_penny
  GHC-Options: -Wall
  if flag(debug)
    GHC-Options: -rtsopts -auto-all -caf-all

  if ! flag(build-diff)
    buildable: False

Flag build-diff
  Description: Build the penny-diff executable
  Default: True

Executable penny-reprint
  Build-depends:
      penny == pv_penny
    , base >= pv_base && < pv_base_max

  hs-source-dirs: bin
  main-is: penny-reprint.hs
  Other-modules: Paths_penny
  ghc-options: -Wall
  if ! flag(build-reprint)
    buildable: False

Flag build-reprint
  Description: Build the penny-reprint executable
  Default: True

Executable penny-reconcile
  Build-depends:
      penny == pv_penny
    , base >= pv_base && < pv_base_max

  hs-source-dirs: bin
  main-is: penny-reconcile.hs
  Other-modules: Paths_penny
  ghc-options: -Wall
  if ! flag(build-reconcile)
    buildable: False

Flag build-reconcile
  Description: Build the penny-reconcile executable
  Default: True

Flag debug
  Description: turns on some debugging options
  Default: False

Flag test
  Description: enables QuickCheck tests
  Default: False

Flag incabal
  Description: enables imports that only Cabal makes available
  Default: True

