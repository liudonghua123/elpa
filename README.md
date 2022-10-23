Jarchive teaches emacs how to navigate to files inside jars.

With it enabled things like this will open up `page.clj` in a read-only buffer.

``` emacs-lisp
(find-file "~/.m2/repository/hiccup/hiccup/1.0.5/hiccup-1.0.5.jar::/hiccup/page.clj")
```

When using eglot, invoking `xref-find-definitions` on symbols defined in JAR files should correctly open the files inside the jars.

This package is intended to be used with eglot and lsp-servers like clojure-lsp that provide URIs to files contained in jars through methods like `textDocument/definition`.

# Installing

Add this repository to your emacs load-path

``` emacs-lisp
(add-to-list 'load-path "path/to/jarchive")
```

Then load and initialize the repository

``` emacs-lisp
(require 'jarchive)
(jarchive-setup)
```

It can also be done in a hook (recommended)

``` emacs-lisp
(add-hook 'clojure-mode-hook #'jarchive-setup)
```

or interactively, via `M-x jarchive-setup`.

## Note about when to call `jarchive-setup`

Some Emacs "distributions" like doom set the `file-name-handler-alist` var to nil on startup, then restore it's value when startup is complete.

If this is the case for you, `jarchive-setup` should be called AFTER everything is initialized.
This package modifies `file-name-handler-alist`, so it relies on it not being reset after `jarchive-setup` is invoked.
