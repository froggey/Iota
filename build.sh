#!/bin/bash

set -x
set -e

TOOLDIR=`pwd`/toolchain

# Remove existing build directories before building.
rm -rf newlib-build sdl-build prboom-build sdlquake-build
rm -rf ${TOOLDIR}/le32-iota

mkdir newlib-build
pushd newlib-build
CC_FOR_TARGET=${TOOLDIR}/bin/clang AR_FOR_TARGET=${TOOLDIR}/bin/llvm-ar RANLIB_FOR_TARGET=true ../newlib-2.5.0.20170228/configure --target=le32-iota --enable-newlib-io-c99-formats --prefix=${TOOLDIR}
LLVM_LINK=${TOOLDIR}/bin/llvm-link make ${MAKEFLAGS}
make install
popd

mkdir sdl-build
pushd sdl-build
cp ../sdl-config.cache config.cache
CC=${TOOLDIR}/bin/clang ../SDL-1.2.15/configure -C --host=le32-iota --prefix=${TOOLDIR}/le32-iota
LLVM_LINK=${TOOLDIR}/bin/llvm-link make ${MAKEFLAGS}
make install
popd

mkdir prboom-build
pushd prboom-build
cp ../prboom-config.cache config.cache
CC=${TOOLDIR}/bin/clang ../prboom-2.5.0/configure -C --host=le32-iota --prefix=${TOOLDIR}/le32-iota --disable-gl
pushd src
pushd SDL
make libsdldoom.a
popd
LLVM_LINK=${TOOLDIR}/bin/llvm-link make prboom ${MAKEFLAGS}
popd
popd
${TOOLDIR}/bin/iota -optimize="speed (safety 0) (debug 0) (space 0) (compilation-speed 0)" -package=":prboom" prboom-build/src/prboom > prboom.lisp

mkdir sdlquake-build
pushd sdlquake-build
CC=${TOOLDIR}/bin/clang ../sdlquake-1.0.9/configure --host=le32-iota --prefix=${TOOLDIR}/le32-iota --disable-sdltest --with-sdl-prefix=${TOOLDIR}/le32-iota
LLVM_LINK=${TOOLDIR}/bin/llvm-link make
popd
${TOOLDIR}/bin/iota -optimize="speed (safety 0) (debug 0) (space 0) (compilation-speed 0)" -package=":sdlquake" sdlquake-build/sdlquake > sdlquake.lisp
