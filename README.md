Autocrypt for Emacs
===================

[Autocrypt][autocrypt] is cryptography protocol, for distributing and
automatically encrypting emails. This package generically implements
the protocol, for various Emacs MUAs.

Currently, it supports:

- Rmail, as a viewer
- Gnus, as a viewer
- mu4e, as a viewer
- message, as a composer

As of writing, this package doesn't fully implement the autocrypt
protocol. It is currently still missing:

- Composing the setup message
- Parsing the setup message
- Key-Gossip Parsing (although the logic has been implemented)

I will attempt to on these issues in time, but any contributions, both
in terms of extending the support of the protocol, but also MUA
integration is welcomed.

How to use
----------

This package is published on [MELPA]. Using `use-package`, one might
configure `autocrypt.el` to read headers using Rmail and inject
headers using `message-mode` as follows:

	(use-package autocrypt
	  :hook ((rmail-mode . autocrypt-mode)
	         (message-mode . autocrypt-mode)))

Autocrypt recommends using a new or separate key pair for signing and
encrypting. If you wish to do so, call the `autocrypt-create-account`
command. In case you want to manually configure your setup, customise
the `autocrypt-accounts` option. Note that configuring a key is
necessary for `autocrypt.el` to function properly.

Bug reports and patches should be sent to my [public inbox].

Copying
-------

`autocrypt.el` is distributed under the [CC0 1.0 Universal (CC0 1.0)
Public Domain Dedication][cc0] license.

[autocrypt]: https://autocrypt.org/
[public inbox]: https://lists.sr.ht/~zge/public-inbox
[MELPA]: https://melpa.org/#/autocrypt
[cc0]: https://creativecommons.org/publicdomain/zero/1.0/deed
