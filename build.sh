#!/bin/sh
# Build and install executables for WisiToken grammar mode.

export GPR_PROJECT_PATH="../wisi-2.2.1"

gnatprep -DELPA="yes" wisitoken_grammar.gpr.gp wisitoken_grammar.gpr

gprbuild -p -P wisitoken_grammar.gpr
gprinstall -f -p -P wisitoken_grammar.gpr --install-name=wisitoken_grammar

# end of file
