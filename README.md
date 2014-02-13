Welcome to Penny, double-entry accounting.

Penny's web pages are at:

http://massysett.github.com/penny
http://hackage.haskell.org/package/penny
http://github.com/massysett/penny

Versions that contain at least one odd number are development
versions. They are not posted to Hackage. I try to keep the master
branch in compilable shape. However, development versions may not pass
all tests, and in particular they may have out of date or incomplete
documentation.

Releases consist of code of reasonable quality. All of the groups in
their release numbers are even.

Penny is licensed under the MIT license, see the LICENSE file.

To install the latest release, "cabal install penny" should work.  To
also build test executables, run "cabal install -ftest penny".  That
will give you two additional executables: penny-test, which when run
will test a bunch of QuickCheck properties, and penny-gibberish, which
prints a random, but valid, ledger file.

To install the manual pages and the documentation, run "sh
install-docs". It will install the manual pages to $PREFIX/share/man
and the other documentation to $PREFIX/share/doc/penny. By default
$PREFIX is /usr/local; you can change this by editing the
install-docs file and changing the "PREFIX" variable.

To remove the manual pages and the documentation, run "sh
install-docs remove."

The first thing you will want to look at is the manual page
penny-basics(7).  Then you will want to examine the starter.pny file
in the examples directory, which will show you how to write a ledger
file. penny-suite(7) will then direct you to other documentation that
may interest you.

Though I do use this program to maintain all my financial records, it
is still relatively new and no one but me has tested it. Use at your
own risk.

[![Build Status](https://travis-ci.org/massysett/penny.png?branch=master)](https://travis-ci.org/massysett/penny)
