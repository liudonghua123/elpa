#!/bin/sh
# Install executables for Gpr mode.
#
# See build.sh for build (must be run before install).

if type alr; then
    echo "installing gpr-mode executables via Alire"

    if [ x$1 == x ]; then
        echo "you must specify the install directory with 'install.sh <dir>'"
        return 1
    fi
    # IMPROVEME: when alr supports install, use that
    cp emacs_gpr_mode*/bin/* $1

elif type gprbuild; then
    echo "installing gpr-mode executables via gnat compiler"
    
    # $1 : optional --prefix=<dir>
    #
    # If you don't have write permission in the GNAT installation
    # directory, you need to use --prefix=<dir>, or run with root priviledges.

    if [ -d ../wisi-4.1.? ]; then
        WISI_DIR=`ls -d ../wisi-4.1.?`
    fi
    
    gprinstall -f -p -P gpr_mode_wisi_parse.gpr -aP $WISI_DIR --install-name=gpr_mode_wisi_parse $1 

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file.
