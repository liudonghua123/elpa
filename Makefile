# "Dummy" initial Makefile for GNU Emacs Lisp Package Archive.

setup: GNUmakefile manifest.scm admin

GNUmakefile: admin
	ln -s admin/GNUmakefile ./

manifest.scm: admin
	ln -s admin/elpa-manifest.scm $@

admin:
	git show-ref --verify --quiet remotes/origin/elpa-admin || { \
	    git remote set-branches --add origin elpa-admin;	     \
	    git fetch origin; }
	git worktree add -b elpa-admin admin origin/elpa-admin
