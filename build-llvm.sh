#!/bin/sh

# Build LLVM & clang.
# Not part of build.sh because building LLVM takes forever.

set -x
set -e

TOOLDIR=`pwd`/sysroot

mkdir -p ${TOOLDIR}
cd ${TOOLDIR}
cmake -DCMAKE_INSTALL_PREFIX=${TOOLDIR} -DLLVM_TARGETS_TO_BUILD="" -DLLVM_DEFAULT_TARGET_TRIPLE=le32-iota ../llvm
cmake --build . -- ${MAKEFLAGS}
