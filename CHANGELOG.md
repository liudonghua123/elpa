# Change Log
## 2022-11-11 Release Notes

- Now operates on full jar URIs and zipfile URIs
  - For example `jar:file:///path/to/library.jar!/path/in/jar/source.ext`
  - also `zipfile:///path/to/library.jar::/path/in/jar/source.ext`
- Removed command `jarchive-move-to-visiting-project`
  - Prefer setting `eglot-extend-to-xref` instead, or use `M-x write-file` to save where you like.
- Removed `jarchive--managed-mode`
  - No longer necessary now that `eglot-extend-to-xref` is working properly.
- Works with the latest eglot on emacs master
  - as of commit [1a2d603bb3938ff68ed1a5412d131b41efd40a24](https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=1a2d603bb3938ff68ed1a5412d131b41efd40a24 "Emacs upstream commit 1a2d603bb3938ff68ed1a5412d131b41efd40a24").
- Patches the legacy eglot (version 1.9, released 2022-10-11) on melpa that does not contain the changes from above commit to emacs mainline
  - Current what is available to stable emacs users (version 28.x)
  - https://elpa.gnu.org/packages/eglot.html 
