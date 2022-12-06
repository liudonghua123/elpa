#!/bin/sh
# Install executables for gpr-query
#
# See build.sh for build (must be run before install).

# $1 : optional --prefix=<dir>
#
# If you don't have write permission in the GNAT installation
# directory, you need to use --prefix=<dir>, or run with root
# privileges.

# We use 'gprinstall', because it defaults to the gnat compiler
# install directory; gpr_query relies on the format of the *.ali
# files, so it is tightly tied to the compiler version.

if type alr; then
    alr exec -- gprinstall -f -p -P emacs_gpr_query.gpr --install-name=emacs_gpr_query $1

elif type gprbuild; then
    gprinstall -f -p -P emacs_gpr_query.gpr --install-name=emacs_gpr_query $1 

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file.
