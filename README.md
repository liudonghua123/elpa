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

Using MELPA and `use-package`, a minimal setup might look something like
this:

	(use-package face-shift
	  :config
	  (add-hook 'text-mode-hook (face-shift 'yellow '(mail-mode)))
	  (add-hook 'prog-mode-hook (face-shift 'green))
	  (add-hook 'dired-mode-hook (face-shift 'purple)))

The function `face-shift` takes a symbol that tells the function in what
direction the colour should be shifted. This then produces a function
that can be added to a hook. If you don't want anonymous functions in
your looks, consider giving the generated function a name as so:

	(defalias 'my/green-shfit (face-shift 'green))

In the first example, the first two hooks (`text-mode-hook` and
`prog-mode-hook`) are used to derive most text and programming modes,
hence all modes that are based on these will have the same colour
shifts.

But it can also just as easily be applied to a "regular" mode, such as
`dired-mode`.

Also note the second argument to the first `face-shift` call: It is a
list of modes that should _not_ have the transformation applied, even
though the function hook would be called (in this case it is because
`mail-mode` is derived from `text-mode`).

Example
-------

![screenshot]

Bugs
----

- Due to Emacs' implementation of fringes, it's not possible to shift
  the colour of the fringe for just one buffer.
- If the default background colour is dark, the transformations have no
  effect, since the by default try to decrease some RGB values. In that
  case, consider setting `face-shift-inverted` to non-nil.

Any further bugs or questions can be submitted to my [public inbox].

Copying
-------

`face-shfit.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[screenshot]: https://files.catbox.moe/cvm7lm.png
[mailing list]: https://lists.sr.ht/~zge/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
