`bang.el`
=========

`shell-command+` is a `shell-command` substitute that makes it easier
to run commands on regions or whole buffers, among other things.

`shell-command+` has been based on a function named `bang` by [Leah
Neukirchen][leah].

How to use
----------

`shell-command+` is available from [ELPA]. It can be installed by
invoking

	M-x package-install RET shell-command+ RET

Bind the command `shell-command+` to any key, for example `M-!`.

Bug reports and patches should be sent to my [public inbox].

Using [`setup`][setup] I configure `shell-command+` as so:

~~~elisp
(setup (:package shell-command+)
  (:option shell-command+-prompt "$ ")
  (:with-feature dired
    (:bind "M-!" shell-command+))
  (:global "M-!" shell-command+))
~~~

Copying
-------

`bang.el` is distributed under the [CC0 1.0 Universal (CC0 1.0) Public
Domain Dedication][cc0] license.

[leah]: http://leahneukirchen.org/dotfiles/.emacs
[ELPA]: http://elpa.gnu.org/packages/shell-command+.html
[public inbox]: https://lists.sr.ht/~zge/public-inbox
[setup]: http://elpa.gnu.org/packages/setup.html
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
