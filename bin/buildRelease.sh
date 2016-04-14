#!/bin/bash

version=$(sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --eval '(format t "~A" (asdf:component-version (asdf:find-system :strictmath)))' --eval "(quit)")

echo -n "Building version $version, hit enter to continue"
read

mkdir strictmath_$version
cp -ap src/main/* strictmath_$version/
tar zcf strictmath_${version}.tar.gz strictmath_$version/
rm -rf strictmath_$version

echo "All done, it's in strictmath_${version}.tar.gz, you should tag it and push it up to github"
