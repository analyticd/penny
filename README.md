# Penny - double-entry accounting

Penny is a double-entry accounting system which is inspired by, but
incompatible with, Ledger, which is available at

http://ledger-cli.org

# Features

Penny is:

* double-entry accounting.  It does not attempt to hide the
  machinery of double-entry accounting from you; for instance, it
  uses the terms "Debit" and "Credit".  For a refresher on the basics
  of double-entry accounting, pick up a used accounting textbook from
  your favorite bookseller (they can be had cheaply, for less than ten
  U.S. dollars including shipping) or check out

  http://www.principlesofaccounting.com/

  which is a great free online text.

* oriented around the command line and your text editor.

* extensible in Haskell; I have built many ancillary tools that I
  use "in-house" to do things like check the consistency of my
  records.  Included are tools to automatically process and reconcile
  bank statements.

* but fully-featured even if you know no Haskell, and you need not
  learn any Haskell to use Penny.

* logical.  Or at least I hope so.  The command-line syntax attempts
  to be as regular as possible.

* good with Unicode text.

* colorful and can use 256-color terminals.  Reports make great
  effort to use all of your screen space automatically to present
  the most readable reports possible.

* able to handle multiple commodities in a logical and consistent
  way--handy if you wish to track stock or real estate holdings or
  transactions in different currencies.

* obsessive.  If there was a choice between an easy way and a
  correct way, I picked the correct way.  For instance, all
  arithmetic is done using decimal numbers rather than binary
  floating-point types.

* freely licensed under the BSD license.

* intended for use only on UNIX-like operating systems.  In
  particular, the libraries it uses to print things to your terminal
  are available primarily on UNIX.  Maybe it works on Cygwin too; I
  have no idea as I use Penny only on UNIX-like systems.

* tested with QuickCheck.

# Installation

## Dependencies

cabal install will take care of all Haskell dependencies for you;
however, there are also at least two C libraries you will need to
install as Penny depends on other Haskell libraries that use these C
libraries.  You will need to make sure you have the "development"
package installed if you use many Linux distributions; a few
distributors, such as Arch, Slackware, and Gentoo, generally don't
ship separate "development" packages so that won't apply to you.
The C libraries are:

* pcre - http://www.pcre.org/ - on Debian GNU/Linux systems this
  package is called `libpcre3-dev`

* curses - on GNU systems this is known as ncurses,
  http://www.gnu.org/software/ncurses/ Perhaps other, non-GNU curses
  implementations will work as well; I do not know.  On Debian
  GNU/Linux systems, install `libncurses5-dev`.


## Penny itself

To install Penny, get the latest version of the
[Haskell Platform](http://www.haskell.org/platform/)
currently version 2013.2.0.0.  Then do:

    cabal update
    cabal install --enable-tests penny

If you use a Linux distribution, see if your distributor has already
packaged the Haskell Platform for you.

I encourage you to run the tests, so the above commands will do
that, but if you want to skip them, instead just do

    cabal install penny

## Documentation

To install the manual pages and the documentation, you need to find
the downloaded archive.  Typically this is at
`$HOME/.cabal/packages/hackage.haskell.org/penny`.  Unpack this
archive and look inside.  Run

    sh install-docs

It will install the manual pages to `$PREFIX/share/man`
and the other documentation to `$PREFIX/share/doc/penny`. By default
`$PREFIX` is `/usr/local`; you can change this by editing the
install-docs file and changing the `PREFIX` variable.

To remove the manual pages and the documentation, run

    sh install-docs remove

The first thing you will want to look at is the manual page
penny-basics(7).  Then you will want to examine the starter.pny file
in the examples directory, which will show you how to write a ledger
file.  penny-suite(7) will then direct you to other documentation that
may interest you.


# Penny on the Web

Find Penny on Hackage at

http://hackage.haskell.org/package/penny

and on Github at

http://www.github.com/massysett/penny

and web pages are here:

http://massysett.github.io/penny

(I am going to phase out the separate web pages in favor of files in
the main source code tree.)

# Development and bugs

If you find any bugs or if you wish to contribute, please use
[Github](http://www.github.com/massysett/penny) (preferred) or send
email to <omari@smileystation.com> (which works just as well as
Github, but if everything else is equal for you and you would flip a
coin to determine which contact method to use, then please use
Github.)

# Version numbers

Versions are numbered like this: A.B.C.D.  I try to follow the
Haskell Package Versioning Policy, or PVP.  Also, all official
releases have only even numbers for each of A, B, C, and D.
Development releases have an odd number in at least one of A, B, C,
or D.

# Test status

[![Build Status](https://travis-ci.org/massysett/penny.png?branch=master)](https://travis-ci.org/massysett/penny)
