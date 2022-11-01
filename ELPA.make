# For compiling gpr-query Ada code in elpa or devel worktree

#export Standard_Common_Build := Debug

.PHONY : all force

all : build byte-compile autoloads

docs : gpr-query.info

build : config/emacs_gpr_query_config.gpr force
	gprbuild -p -j8 emacs_gpr_query.gpr

install : bin/gpr_query$(EXE_EXT)
	gprinstall -f -p -P emacs_gpr_query.gpr --install-name=gpr_query

ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs
else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"
else
# windows
# specify uniscribe to workaround weird Windows harfbuzz bug
EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe
endif

BYTE_COMPILE := "(progn (setq byte-compile-error-on-warn t)(batch-byte-compile))"
byte-compile : byte-compile-clean
	$(EMACS_EXE) -Q -batch -L . -L $(GNAT_COMPILER) -L $(WISI) --eval $(BYTE_COMPILE) *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (setq generated-autoload-file (expand-file-name \"autoloads.el\"))(update-directory-autoloads \".\"))"

%.info : %.texi
	makeinfo $< -o ../$@

clean : force
	rm -rf gpr-query.info obj gpr_query$(EXE_EXT)

recursive-clean : force
	gprclean -r -P emacs_gpr_query.gpr

### publish to elpa package
ELPA_ROOT ?= $(shell cd ../elpa; pwd -W)

pub : force | $(ELPA_ROOT)/packages/gpr-query
	rm -rf $(ELPA_ROOT)/packages/gpr-query/*
	cp *.el *.gpr *.make *.prj *.sh *.texi $(ELPA_ROOT)/packages/gpr-query
	cp NEWS README $(ELPA_ROOT)/packages/gpr-query
	cd $(ELPA_ROOT)/packages/gpr-query; rm autoloads.el

# builds $(ELPA_ROOT)/archive-devel/*, from the last commit, _not_ the
# current workspace Also checks copyright; run elpa/GNUMakefile
# check/<pkg> first if added files.
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive-devel
	make -C $(ELPA_ROOT)/ build/gpr-query

config/emacs_gpr_query_config.gpr :
	cp emacs_gpr_query_config_devel.gpr config/emacs_gpr_query_config.gpr

### misc stuff
BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.gpr-query)
  TAR_FILE := org.emacs.gpr-query-$(GPR_QUERY_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

zip :
	rm -rf $(TAR_FILE)
	mtn checkout --branch $(BRANCH) $(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 --exclude _MTN -C .. $(TAR_FILE)

tag :
	mtn tag h:org.emacs.gpr-query org.emacs.gpr-query-$(GPR_QUERY_VERSION)


# Local Variables:
# eval: (load-file "prj.el")
# End:
# end of file
