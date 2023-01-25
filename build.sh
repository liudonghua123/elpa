#!/bin/sh
# Build and install executables for WisiToken grammar mode.
#
# See install.sh for install
#
# Copyright (C) 2017 - 2019  Free Software Foundation, Inc.
# This file is part of GNU Emacs.

# wisitoken-grammar-mode is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# wisitoken-grammar-mode is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

if type alr; then
    echo "building wisitoken-grammar-mode executables via Alire"
    alr get emacs_gpr_mode~1.0.1
    cd emacs_gpr_mode_*; alr build --release

elif type gprbuild; then
    echo "building wisitoken-grammar-mode executables via gnat compiler"

    export GPR_PROJECT_PATH=`ls -d ../wisi-4.2.?`

    gnatprep -DELPA="yes" wisitoken_grammar.gpr.gp wisitoken_grammar.gpr

    gprbuild -p -j8 -P wisitoken_grammar.gpr
    gprinstall -f -p -P wisitoken_grammar.gpr --install-name=wisitoken_grammar

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file
