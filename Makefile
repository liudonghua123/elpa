## -*- Mode: makefile -*-
## 
## $Source: /psyc/lang/soar/emacs/utils/dismal/new/Makefile,v $
## 
## Description       : Makefile for dismal
## Original author(s): Frank Ritter	Frank.Ritter@nottingham.ac.uk
## Organization      : Dept. of Psychology, University of Nottingham
## 
## To use this makefile, first check the name of your emacs.  Then:
##
## 1) If the name of your emacs executable is the same as the
##    value of the definition of "EMACS" below, then you can make
##    this system by simply cd-ing to the directory of this file and 
##    typing "make" in a shell.
##
## 2) If the name of your emacs executable is different, then
##    can make this system by typing
##               make EMACS=xxxx
##    where "xxxx" is the name of your emacs executable.
##    or by changing the alias for EMACS defined below.
##
## To release (in case you don't remember):
## (a) change release line below and save this file
## (b) change dismal-version in dismal.el and save the file
## (c) cd ~/tools/emacs/dismal/new/
## (g) change version in LCD-entry
## (h) change verison in README
## (d) type "make" to compile dismal (on keats, upsyc is not set up right)
## (e) type "make package" to make a new directory, tar file, etc.
## (f) change dismal-version in dismal.el back to +
## (g) put copy out on granby
##     ftp granby
##     cd pub
##     put dismal-1.04/dismal-1.04.tar.gz
## (h) put a copy on vpsyc
##     cp ~/tools/emacs/dismal/dismal-1.04/dismal-1.04.tar.gz ~/ftp-ritter/dismal-1.04.tar.gz
## Optional arguments to make:
##    package - same as no argument
##    clean   - remove all .elc files and any other cruft


# First define all of our aliases.
# Beware, if "emacs" is aliased to "emacs -q", then -batch below will be 
# treated as a file.
EMACS    = emacs
COMPILER = cc

# no user changeable variables below here.
RELEASE = dismal-1.04

# main files that will be loaded during compiles
# in .elc format.
OBJS    = dismal.elc \
	  dismal-simple-menus.elc \
	  dismal-metacolumn.elc \
	  dismal-mouse-x.elc \
	  dismal-mode-defaults.elc \
	  auto-aligner.elc \
	  dismal-model-extensions.elc \
	  semi-coder.elc

# supporting files that will be used as utilities
# in .elc format.
EXTRAOBJS = float.elc \
          float-changes.elc \
          vectors.elc \
          heaps.elc \
          rmatrix.elc \
          dismal-data-structures.elc \
          soar-misc.elc \
          ritter-math.elc \
          insert-date.elc \
          simple-menu.elc \
          goto-manual.elc \
	  keystroke.elc \
	  popper.elc \
	  log.elc \
	  dismal-mode-defaults.elc

# files that will be compiled
SRCS    = dismal-data-structures.el \
	  dismal.el \
	  dismal-simple-menus.el \
          dismal-metacolumn.el \
          dismal-mouse-x.el \
          float.el \
          popper.el \
          float-changes.el \
          vectors.el \
          heaps.el \
          rmatrix.el \
          dismal-mode-defaults.el \
          ritter-math.el \
          soar-misc.el \
          insert-date.el \
          simple-menu.el \
	  auto-aligner.el \
	  dismal-model-extensions.el \
	  semi-coder.el \
	  keystroke.el \
	  make-km-aliases.el \
	  log.el \
	  emergency.el \
          goto-manual.el

# files along for the ride not compiled
MISC 	= COPYING \
	  Makefile \
	  README \
	  REFCARD \
	  LCD-entry \
	  example-codes.txt \
	  aligner-test-data.txt \
	  keystroke4.dis \
	  simple-keystroke.dis \
	  test.dis \
	  checkout-dismal.script \
	  dismal-manual.tex \
	  dismal.info \
	  dismal.info-1 \
	  dismal.info-2 \
	  timer.c \
	  popper.lcd \
	  dismal-manual.ps
#	  dismal-manual.rtf \

all:	${EXTRAOBJS} ${OBJS} tags log

# "make package" will make this package.
# get copies of the util files moved up
# then clean up the old tar file and make new one
package: ${SRCS} ${MISC}
	rm -fr ../${RELEASE}/*
	rm -fr ../${RELEASE}
	mkdir ../${RELEASE}
#	Copy latest utilities up
	cp ./utilities/goto-manual.el .
	cp ./utilities/insert-date.el .
	cp ./utilities/ritter-math.el .
	cp ./utilities/simple-menu.el .
	cp ./utilities/soar-misc.el .
	cp ./utilities/x-mouse.el .
	cp ./manuals6/* .
	cp ${SRCS} ../${RELEASE}
	cp ${MISC} ../${RELEASE}
#	cp ${OBJS} ../${RELEASE}
#	rm ./goto-manual.el
#	rm ./insert-date.el
#	rm ./ritter-math.el
#	rm ./simple-menu.el
#	rm ./soar-misc.el
#	rm ./x-mouse.el
	cd ..; tar clf ${RELEASE}.tar ${RELEASE}/*
	cp ../${RELEASE}.tar ../${RELEASE}
	cd ..; gzip ${RELEASE}.tar
	mv ../${RELEASE}.tar.gz ../${RELEASE}
	chmod og+xr ../${RELEASE}
	chmod og+r ../${RELEASE}/*

clean:
	rm -f ${OBJS} ${EXTRAOBJS} TAGS

tags:
	etags *.el

log:
	${COMPILER} timer.c -o timer.bin

.SUFFIXES: .elc .el .tar .Z

${EXTRAOBJS}:
	${EMACS} -batch -q -f batch-byte-compile $(@:.elc=.el)


# dismal-mode-defaults is most important, for it makes sure that current 
# directory is on load-path
BASICLOADS =  -l ./dismal-mode-defaults.elc dismal-data-structures.elc \
	./popper.elc ./float.elc \
	./float-changes.elc ./vectors.elc ./heaps.elc ./rmatrix.elc \
	./ritter-math.elc ./soar-misc.elc \
	./insert-date.elc ./simple-menu.elc \
	./goto-manual.elc

.el.elc:
	${EMACS} -batch -q ${BASICLOADS} -f batch-byte-compile $(@:.elc=.el)

# Special rules.

#dismal.elc:  
#	${EMACS} -batch -q -l ./popper.elc ./float.elc \
#		./float-changes.elc -f batch-byte-compile dismal.el

# Dependencies.

#dismal.elc:            dismal-simple-menus.elc dismal-metacolumn.elc \
#		float.elc float-changes.elc heaps.elc dismal-mouse-x.elc \
#		dismal-mode-defaults.elc popper.elc vectors.elc rmatrix.elc \
#		utilities/insert-date.elc \
#		utilities/simple-menu.elc utilities/goto-manual.elc dismal.el
dismal-simple-menus.elc:  dismal.elc ./dismal-simple-menus.el 
dismal-mouse-x.elc:    dismal.elc dismal-mouse-x.el

## Not used yet: (taken from edb makefile)
## info: database.texi
## 	makeinfo -o edb.info database.texi
## 	texi2dvi database.texi
## 	makeinfo -o dismal.info dismal-manual.tex
## 	texi2dvi dismal-manual.tex
