EMACS = emacs

all: num3-mode.elc

%.elc : %.el
	@echo Compiling $<
	$(EMACS) -batch -q -no-site-file -L . -f batch-byte-compile $<

test: all
	$(EMACS) -batch -L . -l test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f -- *.elc

.PHONY: all clean test
