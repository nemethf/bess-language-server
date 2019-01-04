Bess Language Server
====================


bessls is a simple modification of
[pyls](https://github.com/palantir/python-language-server).  See the
original [README](README.orig.rst).

The [Bess](https://github.com/NetSys/bess) scripting/configuration
language is basically python with some syntactic sugar and global
variables.  bessls relies on
[bess-gen-doc](https://github.com/nemethf/bess-gen-doc) for the global
variables (see [mclass.py](bessls/extra/mclass.py)).  It uses very
simple heuristics to understand the syntactic sugar.

The BESS environment variable should be set to path of the bess source
code.

bessls should work with any text editor supporting the [language
server protocol](https://langserver.org/), however I only tested it
with emacs/[eglot](https://github.com/joaotavora/eglot).

Adding the following lines to your [Emacs initialization
file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)
is enough.  However, there are further configuration ideas in
[bess.el].

```elisp
(setenv "BESS" "/opt/bess")

(define-derived-mode bess-mode python-mode "bess")
(add-to-list 'auto-mode-alist '("\\.bess\\'" . bess-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(bess-mode . ("bessls"))))
```
