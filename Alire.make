# For compiling gpr-query Ada code with Alire

include ../wisi/alire_rules.make

install : bin/gpr_query$(EXE_EXT)
	gprinstall -f -p -P emacs_gpr_query.gpr --prefix=~/.local --install-name=gpr_query

# Local Variables:
# eval: (load-file "prj-eglot.el")
# End:
