#!/bin/bash -e

mkdir -p tmp/sbcl

cwd=$PWD
( cd tmp &&
  tar jxf ../deps/tarpit/sbcl-1.2.6-x86-64-linux-binary.tar.bz2 &&
  cd sbcl-1.2.6-x86-64-linux/ &&
  SBCL_HOME="" INSTALL_ROOT=$cwd/tmp/sbcl/ bash install.sh )

mkdir -p tmp/deps/

( cd tmp/deps &&
  tar zxf ../../deps/tarpit/cl-ppcre.tar.gz &&
  tar zxf ../../deps/tarpit/style-checker_0.1.tar.gz &&
  ln -s cl-ppcre-2.0.10/cl-ppcre.asd . &&
  ln -s style-checker_0.1/style-checker.asd .
)


SBCL_HOME="" tmp/sbcl/bin/sbcl --core tmp/sbcl/lib/sbcl/sbcl.core --no-sysinit --no-userinit \
  --eval "(require 'asdf)" \
  --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"${PWD}/tmp/deps\") :IGNORE-INHERITED-CONFIGURATION))" \
  --eval "(asdf:load-system :ieee-floats)" \
  --eval "(asdf:load-system :style-checker)" \
  --eval "(asdf:load-system :docgen)" \
  --eval "(asdf:clear-output-translations)" \
  --eval '(sb-ext:save-lisp-and-die "travissbcl" :executable t)' \

chmod +x travissbcl
travisname=travissbcl-$(git rev-parse --short HEAD)
mv travissbcl $travisname

echo "You should upload via the command: scp $travisname nami:/var/travis/sbcls/strictmath/"
echo "You should also set travisname in .travis.yml to $travisname"

rm -rf tmp
