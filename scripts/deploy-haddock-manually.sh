#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone git@github.com:relrod/bioparse.git "$f/bioparse.git"
cabal haddock
cabal install --enable-bench
pushd "$f/bioparse.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/bioparse/* "$f/bioparse.git/"
cabal bench --benchmark-option="-o$f/bioparse.git/benchmarks.html"
pushd "$f/bioparse.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://relrod.github.io/bioparse/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
