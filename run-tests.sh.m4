#!/bin/sh

set -e
set -v

# Don't do:
# cabal install 
# without the --only-dependencies flag.  That will run the tests,
# but then the test output does not go to console.  Instead, run
# cabal install --only-dependencies --enable-tests first, then cabal
# build, then run the tests manually.

ghc --version
git rev-parse HEAD
date

# make dist directory
rm -rf test-dist
cabal sdist --output-directory=test-dist
cd test-dist

cabal sandbox delete || true
cabal sandbox init
cabal install \
    --only-dependencies \
    --disable-library-profiling \
    --disable-executable-profiling \
    --enable-tests \
    -v \
    '--constraint=base           ==pv_base' \
    '--constraint=bytestring     ==pv_bytestring' \
    '--constraint=containers     ==pv_containers' \
    '--constraint=old-locale     ==pv_old_locale' \
    '--constraint=time           ==pv_time' \
    '--constraint=parsec         ==pv_parsec' \
    '--constraint=QuickCheck     ==pv_QuickCheck' \
    '--constraint=split          ==pv_split' \
    '--constraint=text           ==pv_text' \
    '--constraint=transformers   ==pv_transformers' \
    '--constraint=anonymous-sums ==pv_anonymous_sums' \
    '--constraint=matchers       ==pv_matchers' \
    '--constraint=multiarg       ==pv_multiarg' \
    '--constraint=ofx            ==pv_ofx' \
    '--constraint=prednote       ==pv_prednote' \
    '--constraint=rainbow        ==pv_rainbow' \
    '--constraint=action-permutations ==pv_action_permutations' \
    '--constraint=cereal         ==pv_cereal' \
    '--constraint=either         ==pv_either' \
    '--constraint=pretty-show    ==pv_pretty_show' \
    '--constraint=random         ==pv_random' \
    '--constraint=random-shuffle ==pv_random_shuffle' \
    '--constraint=semigroups     ==pv_semigroups'
echo run-tests.sh - package cache before build:
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
dist/build/penny-test/penny-test
