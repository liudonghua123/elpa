# cpupower-el

An emacs lisp wrapper around `cpupower`

Using quelpa:
```lisp
(quelpa
 '(cpupower :fetcher gitlab
            :repo "steve-emacs-stuff/cpupower-el"
            :branch "main"))
(require 'cpupower)
```

Running `M-x cpupower-info` or `M-x cpupower-set-governor` will
probably be what you want to do.
