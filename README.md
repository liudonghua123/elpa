Guess and insert C headers
==========================

Find here the source for auto-header.el, an Emacs minor mode that can
automatically guess what headers to prepend to a C file using
information from Man pages.

**Note:** The insertion mechanism is currently primitive and might
mess up files with fragile or complicated `#ifdef` constructs.

Installation
------------

Auto-header.el is avaliable from [GNU ELPA]. It can be installed by
invoking

	M-x package-install RET auto-header RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/auto-header.html

Usage
-----

You can have auto-header run every time a buffer is saved, by
modifying `before-save-hook` using the `auto-header-mode` minor mode:

    (add-hook 'c-mode-hook #'auto-header-mode)

Keep in mind that auto-header.el was only written for C in mind, and
will emit a warning when used with other major modes.

Contribute
----------

As auto-header.el is distribed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial
code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Source code
-----------

Auto-header.el is developed on [SourceHut].

[SourceHut]:
	https://git.sr.ht/~pkal/auto-header

Bugs and Patches
----------------

Bugs, patches, comments or questions can be submitted to my [public
inbox].

[public inbox]:
	https://lists.sr.ht/~pkal/public-inbox

Distribution
------------

Auto-header.el and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html
