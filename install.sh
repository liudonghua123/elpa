#!/bin/sh
# Install executables for gpr-query
#
# See build.sh for build (must be run before install).

# $1 : optional <install dir>
#
# If you don't have write permission in the GNAT installation
# directory, you need to use --prefix=<dir>, or run with root
# privileges.

if [ x$1 = x ]; then
    PREFIX=$HOME/.local        
    # as recommended by https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
else
    PREFIX=$1
fi
    
echo "installing gpr-query executables to" $PREFIX/bin

if type alr; then
    cp emacs_gpr_query/bin/* $PREFIX/bin

elif type gprbuild; then
    cp bin/* $PREFIX/bin

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file.
