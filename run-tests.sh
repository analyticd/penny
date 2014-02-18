#!/bin/sh

set -e
set -v

# Don't do:
# cabal install 
# without the --only-dependencies flag.  That will run the tests,
# but then the test output does not go to console.  Instead, run
# cabal install --only-dependencies --enable-tests first, then cabal
# build, then run the tests manually.

cabal sandbox delete || true
cabal sandbox init
cabal install \
    --only-dependencies \
    --disable-library-profiling \
    --disable-executable-profiling \
    --enable-tests \
    -v \
    '--constraint=base           ==4.6.0.1' \
    '--constraint=bytestring     ==0.10.0.2' \
    '--constraint=containers     ==0.5.0.0' \
    '--constraint=old-locale     ==1.0.0.5' \
    '--constraint=time           ==1.4.0.1' \
    '--constraint=parsec         ==3.1.3' \
    '--constraint=QuickCheck     ==2.5' \
    '--constraint=split          ==0.2.2' \
    '--constraint=text           ==0.11.3.1' \
    '--constraint=transformers   ==0.3.0.0' \
    '--constraint=anonymous-sums ==0.2.0.0' \
    '--constraint=matchers       ==0.14.0.0' \
    '--constraint=multiarg       ==0.24.0.0' \
    '--constraint=ofx            ==0.4.0.0' \
    '--constraint=prednote       ==0.18.0.0' \
    '--constraint=rainbow        ==0.6.0.0' \
    '--constraint=action-permutations ==0.0.0.0' \
    '--constraint=cereal         ==0.3.5.2' \
    '--constraint=either         ==3.4.1' \
    '--constraint=pretty-show    ==1.5' \
    '--constraint=random         ==1.0.1.1' \
    '--constraint=random-shuffle ==0.0.4' \
    '--constraint=semigroups     ==0.9.2'
echo run-tests.sh - here is the state of the package cache
echo before building penny:
cabal sandbox hc-pkg list

# the cabal install --only-dependencies does NOT configure
# the package
cabal configure --disable-library-profiling \
    --disable-executable-profiling \
    -v \
    --enable-tests
cabal build -v
echo run-tests.sh - state of package cache after building penny:
cabal sandbox hc-pkg list
