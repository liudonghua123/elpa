#!/bin/sh
# Build the gpr_query executable.
#    build.sh options when not using Alire; <other gprbuild options>
#    e.g. 'build.sh -j0' : use all available processors to compile
#         'build.sh -wn' : treat warnings as warnings.
#         'build.sh -vh' : Verbose output (high verbosity)
#
# See install.sh for install

if type alr; then
    # alr can be installed from https://alire.ada.dev/
    echo "building gpr_query via Alire; compiler:"
    alr toolchain
    alr get emacs_gpr_query~1.0.1
    cd emacs_gpr_query_*; alr build --release

elif type gprbuild; then
    echo "building gpr_query via gnat compiler"

    # As of gnat pro 21, gnat_util is no longer provided or required
    echo 'with "gnat_util"; abstract project check is end check;' > check.gpr
    gprbuild -P check.gpr > /dev/null 2>&1
    if test $? -eq 0 ; then
        HAVE_GNAT_UTIL=yes
    else
        HAVE_GNAT_UTIL=no
    fi

    echo "gnatprep " $args
    gnatprep $args

    mkdir -p config
    cp emacs_gpr_query_config_release.gpr config/emacs_gpr_query_config.gpr

    # Allow running build.sh again, since it often fails the first time.
    #  - Run gprclean, to allow changing compilers and other drastic things

    gprclean -r -P emacs_gpr_query.gpr

    gprbuild -p -j8 -P emacs_gpr_query.gpr "$@"

else
    echo "neither Alire nor gnat compiler found"
    exit 1
fi

# end of file
