Bess Language Server
====================


bess-ls is a simple modification of
[pyls](https://github.com/palantir/python-language-server).  See the
original [README](README.orig.rst).

[Bess](https://github.com/NetSys/bess) is basically python with some
syntactic sugar and global variables.  bess-ls relies on
[bess-gen-doc](https://github.com/nemethf/bess-gen-doc) for the global
variables (see [mclass.py](bessls/extra/mclass.py)).  It uses very
simple heuristics to understand the syntactic sugar.


