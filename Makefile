## -*- Mode: makefile -*-
## 
## $Source: /psyc/lang/soar/emacs/utils/dismal/new/Makefile,v $
## 
## Description       : Makefile for dismal
## Original author(s): Frank Ritter	ritter@psyc.nott.ac.uk
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

# first define all of our aliases
EMACS   = emacs

# no user changeable variables below here.
RELEASE = 0.86

# main files that will be loaded
# in .elc format.
OBJS    = dismal.elc \
          dismal-simple-menus.elc \
          dismal-metacolumn.elc \
          dismal-mouse-x.elc \
          dismal-mode-defaults.elc \

# supporting files that will be used as utilities
# in .elc format.
EXTRAOBJS = float.elc \
          float-changes.elc \
          vectors.elc \
          heaps.elc \
          rmatrix.elc \
          soar-misc.elc \
          ritter-math.elc \
          insert-date.elc \
          simple-menu.elc \
          goto-manual.elc \
	  popper.elc

# files that will be compiled
SRCS    = dismal.el \
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
          goto-manual.el

# files along for the ride not compiled
MISC 	= COPYING \
	  Makefile \
	  README \
	  REFCARD \
	  dismal-mode.doc \
	  dismal-mode.mss \
	  test.dis \
	  checkout-dismal.script \
	  popper.lcd

all:	${EXTRAOBJS} ${OBJS} tags

# "make package" will make this package.
# get copies of the util files moved up
# then clean up the old tar file and make new one
package: ${SRCS} ${MISC}
	cp ./utilities/goto-manual.el .
	cp ./utilities/insert-date.el .
	cp ./utilities/ritter-math.el .
	cp ./utilities/simple-menu.el .
	cp ./utilities/soar-misc.el .
	cp ./utilities/x-mouse.el .
	rm -f dismal.tar.* dismal.tar.Z.* dismal.tar.Z.uu.*
	tar clf dismal.tar ${SRCS} ${MISC}
	compress dismal.tar
	uuencode dismal.tar.Z dismal.tar.Z > dismal.tar.Z.uu
	mv dismal.tar.Z ../${RELEASE}/dismal.tar.Z.${RELEASE}
	mv dismal.tar.Z.uu ../${RELEASE}/dismal.tar.Z.uu.${RELEASE}
	cp ${SRCS} ../${RELEASE}
	cp ${MISC} ../${RELEASE}
	cp ${OBJS} ../${RELEASE}
#	rm ./utilities/goto-manual.el .
#	rm ./utilities/insert-date.el .
#	rm ./utilities/ritter-math.el .
#	rm ./utilities/simple-menu.el .
#	rm ./utilities/soar-misc.el .
#	rm ./utilities/x-mouse.el .

clean:
	rm -f ${OBJS} ${EXTRAOBJS} TAGS

tags:
	etags *.el

.SUFFIXES: .elc .el .tar .Z .uu

${EXTRAOBJS}:
	${EMACS} -batch -q -f batch-byte-compile $(@:.elc=.el)


# dismal-mode-defaults is most important, for it makes sure that current directory 
# is on load-path
BASICLOADS =  -l ./dismal-mode-defaults.elc ./popper.elc ./float.elc \
 	 ./float-changes.elc ./vectors.elc ./heaps.elc ./rmatrix.elc \
         ./ritter-math.elc ./soar-misc.elc \
         ./insert-date.elc ./simple-menu.elc \
         ./goto-manual.elc

.el.elc:
	${EMACS} -batch -q ${BASICLOADS} -f batch-byte-compile $(@:.elc=.el)

# Special rules.

#dismal.elc:  
#	${EMACS} -batch -q -l ./popper.elc ./float.elc ./float-changes.elc -f batch-byte-compile dismal.el

# Dependencies.

#dismal.elc:            dismal-simple-menus.elc dismal-metacolumn.elc float.elc \
#		float-changes.elc heaps.elc dismal-mouse-x.elc dismal-mode-defaults.elc \
#		popper.elc vectors.elc rmatrix.elc utilities/soar-misc.elc \
#		utilities/insert-date.elc utilities/simple-menu.elc utilities/goto-manual.elc \
#		dismal.el
dismal-simple-menus.elc:  dismal.elc ./dismal-simple-menus.el 
dismal-mouse-x.elc:    dismal.elc dismal-mouse-x.el
