#!/bin/sh
# requires syb, happy, alex
# happy, alex on path
set -e
git clone https://github.com/kovach/mm.git
cabal unpack language-c-0.4.7
cd language-c-0.4.7
patch -p1 < ../language-c-0.4.7.patch
cd ..
cabal sandbox init
cabal sandbox add-source language-c-0.4.7
cabal sandbox add-source mm/simplify
# needed for language-c:
# cabal install syb happy alex
# add happy/alex to the path
cabal install --dependencies-only
cabal build
