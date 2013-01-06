-- | This file is a sample of how to configure penny-fit. You will
-- have to adapt it for your own needs. Rename the file to
-- FILENAME.hs, where FILENAME is the name you want for your program
-- (an easy choice is "penny-fit".) Then, compile the file with
--
-- ghc --make FILENAME
--
-- and if all goes well you will have a program to use.
module Main where

import Penny.Brenner
import qualified Penny.Brenner.Amex
import qualified Penny.Brenner.BofA

-- | The Config type configures all the financial institution accounts.
config :: Config
config = Config
  { defaultFitAcct = Nothing
    -- ^ If you have a default financial institution account, use Just
    -- ACCOUNT here. For example, if I wanted to use my amex account
    -- by default, I would put @Just amex@ here. I don't want to use
    -- an account by default; I want to be required to explicitly
    -- state an account, so I put Nothing here.

  , moreFitAccts = [ ("amex", amex)
                   , ("chkh", houseChecking)
                   , ("chko", omariChecking)
                   ]
    -- ^ This is a list of financial institution accounts in addition
    -- to the default (if you have one.) Each of these is a pair. The
    -- first element is a string with the name of the account, as you
    -- will specify it from the command line. The second element is a
    -- FitAcct that configures the account.
  }

amex :: FitAcct
amex = FitAcct
  { dbLocation = "/home/massysett/ledger/amex-db"
    -- ^ The location of the database of financial institution
    -- postings. You can make this path relative, in which case it is
    -- interpreted relative to the current directory at runtime, or
    -- absolute. No expansion of tildes, environment variables,
    -- etc. is performed.

  , pennyAcct = "Liabilities:Current:Amex"
    -- ^ Postings from this financial institution appear under this
    -- account in your ledger file(s).

  , defaultAcct = "Expenses:Unclassified"
    -- ^ When penny-fit finds a financial institution posting and it
    -- does not have a matching posting in your ledger, it must create
    -- a new transaction with two postings. One posting will be in the
    -- pennyAcct specified above and the other posting will be in this
    -- account.

  , currency = "$"
    -- ^ All postings will be in this currency

  , groupSpecs = GroupSpecs NoGrouping NoGrouping
    -- ^ When penny-fit reprints your ledger, it uses this to
    -- determine how to perform digit grouping for the quantities in
    -- the output. This takes the form
    --
    -- GroupSpecs G1 G2
    --
    -- where G1 and G2 can each be
    --
    -- NoGrouping, GroupLarge, or GroupAll
    --
    -- G1 specifies how to group digits to the left of the decimal
    -- point. G2 specifies how to group digits to the right of the
    -- decimal point. All grouping creates groups of three digits.
    --
    -- NoGrouping means do not do any digit grouping at
    -- all.
    --
    -- GroupLarge means perform digit grouping, but only if the
    -- number to be grouped is greater than 9,999 (if grouping to the
    -- left of the decimal point) or if there are more than 4 decimal
    -- places (if grouping to the right of the decimal point.)
    --
    -- GroupAll means group whenever there are at least four digits to
    -- be grouped.

  , translator = IncreaseIsCredit
    -- ^ Postings from your financial institution are specified in
    -- terms of increases or decreases. Postings in your ledger are
    -- specified in terms of debits or credits. The translator
    -- specifies how to convert a posting from your financial
    -- institution to a posting in the pennyAcct in your ledger. For
    -- deposit accounts (e.g. checking) you will typically use
    -- IncreaseIsDebit; for liability accounts (e.g. credit cards) you
    -- will typically use IncreaseIsCredit.

  , side = CommodityOnLeft
    -- ^ When penny-fit creates new postings it must put the commodity
    -- either to the left of the quantity or to the right of the
    -- quantity. Accordingly your choices here are CommodityOnLeft or
    -- CommodityOnRight.

  , spaceBetween = NoSpaceBetween
    -- ^ When penny-fit creates new postings it must decide whether to
    -- put a space between the commodity and the quantity. Accordingly
    -- your choices here are SpaceBetween or NoSpaceBetween.

  , parser = Penny.Brenner.Amex.parser
    -- ^ This determines how to parse the data you have
    -- downloaded. Currently there are only two parsers:
    -- Penny.Brenner.Amex.parser, and Penny.Brenner.BofA.parser. If
    -- you know Haskell you can write new parsers for your bank. Look
    -- at the Haddocks for Penny.Brenner to get started, and use the
    -- two existing parsers for guidance. If you write new parsers,
    -- feel free to give them names within the Penny.Brenner namespace
    -- and upload them to Hackage so that others can use them.
  }

houseChecking :: FitAcct
houseChecking = FitAcct
  { dbLocation = "/home/massysett/ledger/house-checking-db"
  , pennyAcct = "Assets:Current:Checking:House"
  , defaultAcct = "Expenses:Unclassified"
  , currency = "$"
  , groupSpecs = GroupSpecs NoGrouping NoGrouping
  , translator = IncreaseIsDebit
  , side = CommodityOnLeft
  , spaceBetween = NoSpaceBetween
  , parser = Penny.Brenner.BofA.parser
  }

omariChecking :: FitAcct
omariChecking = FitAcct
  { dbLocation = "/home/massysett/ledger/omari-checking-db"
  , pennyAcct = "Assets:Current:Checking:Omari"
  , defaultAcct = "Expenses:Unclassified"
  , currency = "$"
  , groupSpecs = GroupSpecs NoGrouping NoGrouping
  , translator = IncreaseIsDebit
  , side = CommodityOnLeft
  , spaceBetween = NoSpaceBetween
  , parser = Penny.Brenner.BofA.parser
  }

main :: IO ()
main = brennerMain config
