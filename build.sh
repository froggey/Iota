#!/bin/bash

set -x
set -e

TOOLDIR=`pwd`/toolchain

# Remove existing build directories before building.
rm -rf newlib-build sdl-build prboom-build sdlquake-build
rm -rf ${TOOLDIR}/le32-iota

mkdir -p ${TOOLDIR}/le32-iota/lib
cp iota-provided-symbols ${TOOLDIR}/le32-iota/lib/

mkdir newlib-build
pushd newlib-build
CC_FOR_TARGET=${TOOLDIR}/bin/clang AR_FOR_TARGET=${TOOLDIR}/bin/llvm-ar RANLIB_FOR_TARGET=${TOOLDIR}/bin/llvm-ranlib ../newlib-2.5.0.20170228/configure --target=le32-iota --enable-newlib-io-c99-formats --prefix=${TOOLDIR}
make ${MAKEFLAGS}
make install
popd

mkdir sdl-build
pushd sdl-build
CC=${TOOLDIR}/bin/clang ../SDL-1.2.15/configure --host=le32-iota --prefix=${TOOLDIR}/le32-iota
make ${MAKEFLAGS}
make install
popd

mkdir prboom-build
pushd prboom-build
EGREP=egrep CC=${TOOLDIR}/bin/clang CPP="${TOOLDIR}/bin/clang -E" ../prboom-2.5.0/configure --host=le32-iota --prefix=${TOOLDIR}/le32-iota --disable-gl LDFLAGS='-Wl,-optimize=speed+\(safety+0\)+\(debug+0\)+\(space+0\)+\(compilation-speed+0\),-package=:prboom'
make ${MAKEFLAGS}
popd
cp prboom-build/src/prboom prboom.lisp

# There is some weird timing issue that makes make really want to reconfigure.
# Work around this by updating Makefile.in's mtime.
touch sdlquake-1.0.9/Makefile.in
mkdir sdlquake-build
pushd sdlquake-build
CC="${TOOLDIR}/bin/clang -Wl,-optimize=speed+\(safety+0\)+\(debug+0\)+\(space+0\)+\(compilation-speed+0\),-package=:sdlquake" ../sdlquake-1.0.9/configure --host=le32-iota --prefix=${TOOLDIR}/le32-iota --disable-sdltest --with-sdl-prefix=${TOOLDIR}/le32-iota
touch sdlquake-1.0.9/Makefile.in
make ${MAKEFLAGS}
popd
cp sdlquake-build/sdlquake sdlquake.lisp
