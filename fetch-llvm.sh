#!/bin/bash

# Fetch the LLVM & Clang repos, placing them in their proper location.

set -x
set -e

git clone https://github.com/froggey/iota-llvm.git llvm
cd llvm/tools
git clone https://github.com/froggey/iota-clang.git clang
