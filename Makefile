# -*- coding: utf-8; -*-
# Emacs Makefile for Tramp

# Copyright (C) 2019 Free Software Foundation, Inc.

# Author: Michael Albinus <michael.albinus@gmx.de>
# Keywords: comm, processes

# This file is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

EMACS		= emacs
EM		= $(EMACS) -Q -batch -L .
LISP_FILES	= $(wildcard *.el)

.PHONY: all autoloads

.SUFFIXES: .el

all: autoloads

autoloads: $(LISP_FILES)
	$(EM) -l autoload						    \
	  --eval "(setq generate-autoload-cookie \";;;###tramp-autoload\")" \
	  --eval "(setq generated-autoload-file				    \
		    (expand-file-name \"tramp-loaddefs.el\"))"		    \
	  --eval "(setq make-backup-files nil)"				    \
	  -f batch-update-autoloads .
