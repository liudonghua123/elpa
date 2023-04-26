Per Major-Mode Distortion of Buffer Faces
=========================================

Find here the source for the Emacs package `face-shift`.  It allows
Emacs to shift the appearance of each buffer according to their
respective active major modes.

My recommendation is to use no theme when `face-shift` is in use, to
avoid confusion, although it certainly is possible.

Installation
----------

`Face-shift.el` is available from [GNU ELPA].  It can be installed by
invoking

	M-x package-install RET face-shift RET

[GNU ELPA]:
	http://elpa.gnu.org/packages/face-shift.html

Usage
-----

The main entry point is the global minor mode `face-shift-mode`.  It
taints all buffers of a certain major mode according to the user
option `face-shift-shifts`.

Visual Example
--------------

![Screenshot of an Emacs with `face-shift-mode` enabled](https://i.imgur.com/E3tGyNL.png)

Known Issues
------------

- Due to Emacs' implementation of fringes, it's not possible to shift
  the colour of the fringe for just one buffer.

Any further bugs, patches, comments or questions can be submitted to
my [public inbox].

[public inbox]:
	https://lists.sr.ht/~pkal/public-inbox

Contribute
----------

As `face-shift.el` is distribed as part of [GNU ELPA], and therefore
requires a [copyright assignment] to the [FSF], for all non-trivial
code contributions.

[copyright assignment]:
	https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html
[FSF]:
	https://www.fsf.org/

Distribution
------------

`face-shift.el` and all other source files in this directory are
distributed under the [GNU Public License], Version 3 (like Emacs
itself).

[GNU Public License]:
	https://www.gnu.org/licenses/gpl-3.0.en.html

[setup]: http://elpa.gnu.org/packages/setup.html
[mailing list]: https://lists.sr.ht/~pkal/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
