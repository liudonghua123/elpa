`face-shift.el`
===============

`face-shift` lets the user shift the colours in a buffer uniformly
towards a certain hue. This is done by linearly transforming the default
colours of a face, and then applying this transformed/shifted colour
_just_ to this buffer, thus not changing all faces at once.

My recommendation is to use no theme when `face-shift` is in use, to
avoid confusion, although it certainly is possible.

How to use
----------

Using [MELPA][melpa] and [`setup`][setup], a minimal setup might look
something like this:

	(setup (:package face-shift)
	  (:option face-shift-mode t))

The main function for users is `face-shift-mode`, that turns on a global
minor mode, to shift all the faces in all buffers.

This will shift the fore- and background colours of all faces in
`face-shift-faces`, according to `face-shift-shifts`. This variable is
an alist of the form `(MAJOR-MODE . COLOUR-STRING)`. This means that
`face-shift-setup` will iterate the list for so long until the current
major mode is derived from a `MAJOR-MODE`, and will then use
`COLOUR-STRING` to shift all colours. Modify this variable to customise
`face-shift-setup`'s behaviour.

Example
-------

![screenshot]

Bugs
----

- Due to Emacs' implementation of fringes, it's not possible to shift
  the colour of the fringe for just one buffer.

Any further bugs or questions can be submitted to my [public inbox][mailing list].

Copying
-------

`face-shift.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[melpa]: https://melpa.org/#/face-shift
[setup]: http://elpa.gnu.org/packages/setup.html
[screenshot]: https://i.imgur.com/E3tGyNL.png
[mailing list]: https://lists.sr.ht/~pkal/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
