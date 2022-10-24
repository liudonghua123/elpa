# Copyright (C) 2021-2022 Mauro Aranda

# This file is part of hiddenquote.

# hiddenquote is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# hiddenquote is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with hiddenquote.  If not, see <https://www.gnu.org/licenses/>.

## Programs used.
EMACS = emacs
EMACSFLAGS = -batch -f batch-byte-compile

## Variables (some might not be used right now).
PACKAGE = hiddenquote
PACKAGE_BUGREPORT = maurooaranda@gmail.com
PACKAGE_NAME = hiddenquote
PACKAGE_STRING = hiddenquote 1.1
PACKAGE_TARNAME = hiddenquote-1.1
PACKAGE_URL = 
PACKAGE_VERSION = 1.1
DISTDIR = $(PACKAGE_TARNAME)
DISTFILES = COPYING README Makefile hiddenquote.el

## Targets.

.PHONY: all clean dist

all: hiddenquote.elc

hiddenquote.elc: hiddenquote.el
	$(EMACS) $(EMACSFLAGS) hiddenquote.el

clean:
	-rm -f hiddenquote.elc
	-rm -f $(PACKAGE_TARNAME).tar.gz

dist: hiddenquote.elc
	mkdir --parents $(DISTDIR)
	cp --parents $(DISTFILES) $(DISTDIR)
	tar -cf $(PACKAGE_TARNAME).tar $(DISTDIR)
	rm -R $(DISTDIR)
	gzip $(PACKAGE_TARNAME).tar
