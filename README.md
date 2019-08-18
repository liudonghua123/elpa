`bang.el`
=========

`bang` is a `shell-command` substitute that makes it easier to run
commands on regions or whole buffers. This is done by potentially
interpreting the first character differently, as the `bang` docstring
explains.

Bang has been based on a function of the same name by [Leah
Neukirchen][leah].

How to use
----------

Using [MELPA] and `use-package`, a minimal setup might look something like
this:

	(use-package bang
	  :bind ("M-!" . bang))

Bug reports and patches should be sent to my [public inbox].

Copying
-------

`bang.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[leah]: http://leahneukirchen.org/dotfiles/.emacs
[MELPA]: https://melpa.org/#/bang
[public inbox]: https://lists.sr.ht/~zge/public-inbox
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
