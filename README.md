Jarchive teaches emacs how to open project dependencies that reside inside jar files.

# Package status

This package is still under active development and is not yet ready for widespread use. Proceed with caution!

It current works well with eglot on the Emacs master branch. 
If you are using eglot from melpa or the eglot github, this won't work.
I will update the README when this changes.

See the [CHANGELOG](item/CHANGELOG.md "change log") for more information.

# Installing

This package is not available on any package repositories yet.

Write now I recommend cloning this repository and adding it to your emacs load-path:

``` emacs-lisp
(add-to-list 'load-path "path/to/jarchive")
```

There is also an example of a [guix recipe](https://git.sr.ht/~abcdw/rde/tree/90af100a4d70d7016261d39b91b6748768ac374b/rde/packages/emacs-xyz.scm#L330).

Load and initialize the repository

``` emacs-lisp
(require 'jarchive)
(with-eval-after-load "init"
  (jarchive-setup))
```

It can also be done in a hook (recommended)

``` emacs-lisp
(add-hook 'eglot-managed-mode-hook #'jarchive-setup)
```

or interactively, via `M-x jarchive-setup`.

## Note about when to call `jarchive-setup`

Some Emacs "distributions" like doom set the `file-name-handler-alist` var to nil on startup, then restore it's value when startup is complete.

If this is the case for you, `jarchive-setup` should be called AFTER everything is initialized.
This package modifies `file-name-handler-alist`, so it relies on it not being reset after `jarchive-setup` is invoked.

# Usage

With it enabled, things like this will open up `page.clj` in a read-only buffer.

``` emacs-lisp
(find-file "jar:file:///.m2/repository/hiccup/hiccup/1.0.5/hiccup-1.0.5.jar!/hiccup/page.clj")
```

When using eglot connected to a JVM language server, invoking `xref-find-definitions` should correctly open any dependencies that reside in JAR files.

## Other usage considerations

If you want eglot to manage the opened jar'd file in your project's current lsp session, set
``` emacs-lisp
(setq eglot-extend-to-xref t) 
```
This will allow xref to work across your project and the opened file.

If you do not want that, the eglot will probably start a new server to manage the newly opened file.
There are legitimate reasons to do this, because including it in the current LSP session will mean it is included when looking up references.
Large files, like the clojure core library, could create a lot of noise in xref lookups.
Another recommendation if you don't want them managed by eglot is to set
``` emacs-lisp
(setq eglot-autoshutdown t)
```
so that the transient lsp server that is started when opening the file is closed along with it.

## Language server compatibility

I personally only test this with [clojure-lsp](https://clojure-lsp.io/).
I have heard from other users that it also works with some unspecified java language server.
Any language server that provides jar: scheme URIs should be picked up by this package.
If it doesn't, please let me know and I'd be happy to take a look.

## Questions

Questions and patches can be submitted to the [mailing list](https://lists.sr.ht/~dannyfreeman/jarchive-dev).
Any bugs found should include steps to reproduce. 
If possible, and example repository containing a project and instructions (or a nix shell) for installing the language servers would be appreciated.
