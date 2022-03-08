# Meltdown - A common lisp markdown parser
> The story goes like this...

This is just a common lisp parser for markdown (with optional support for Hugo markdown). Unlike the other markdown systems for common lisp that currently exist, this makes no assumptions about what to do with the parsed markdown; it simply returns the AST in the form of CLOS objects for you to do whatever you wish with. It is also written in portable common lisp (i.e. has no external dependencies).
