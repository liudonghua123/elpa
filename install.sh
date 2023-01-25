#!/bin/sh
# Install executables for wisitoken-grammar-mode.
# 
# $1 : optional --prefix=<dir>
#   
# See build.sh for build (must be run before install).

if [ x$1 = x ]; then
    PREFIX=$HOME/.local        
    # as recommended by https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
else
    PREFIX=$1
fi
    
echo "installing wisitoken-grammar-mode executables to" $PREFIX/bin

mkdir -p $PREFIX/bin
cp emacs_gpr_mode*/bin/* $PREFIX/bin

# end of file.
