#!/bin/bash
set -x
Z3_VERSION=4.13.0
case $(uname -s) in
  Linux)
    JOB=build
    Z3_ARCH=x64-glibc-2.31
    ;;
  Darwin)
    JOB=build-mac
    Z3_ARCH=arm64-osx-11.0
    ;;

esac
JOB=build
# c-to-json:
wget -nv --content-disposition "https://gitlab.com/umb-svl/c-to-json/-/jobs/artifacts/main/raw/build/c-to-json-bin.tar.bz2?job=$JOB" -O c-to-json-bin.tar.bz2 &&
tar xvf c-to-json-bin.tar.bz2 &&
rm c-to-json-bin.tar.bz2 &&
# faial:
cp \
  ../faial-bc-dyn \
  ../faial-bc \
  ../c-ast \
  ../faial-drf \
  ../faial-cost \
  bin/ &&
cp ../scripts/faial-drf ../README.md ../LICENSE  ./ &&
# download z3
mkdir lib/ &&
wget -nv --content-disposition "https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VERSION}/z3-${Z3_VERSION}-${Z3_ARCH}.zip" &&
unzip z3-${Z3_VERSION}-${Z3_ARCH}.zip &&
cp z3-${Z3_VERSION}-${Z3_ARCH}/bin/libz3.so lib/ &&
cp z3-${Z3_VERSION}-${Z3_ARCH}/LICENSE.txt ./LICENSE-z3.txt &&
rm -rf z3-${Z3_VERSION}-${Z3_ARCH}.zip z3-${Z3_VERSION}-${Z3_ARCH}/ &&
# display tree & bundle:
tar jcvf faial.tar.bz2 * &&
exit 0
