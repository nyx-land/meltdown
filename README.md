# Meltdown - A Common Lisp markdown parser
> The story goes like this...

This is just a Common Lisp parser for markdown. Unlike the other markdown
systems for Common Lisp that currently exist, this makes no assumptions about
what to do with the parsed markdown; it simply returns the AST in the form of
CLOS objects for you to do whatever you wish with. It is also written in
portable Common Lisp* (i.e. has no external dependencies).

Meltdown trades the convenience of other systems that convert markdown to HTML
for maximum flexibility by making it possible to treat markdown documents as
rich CLOS objects and spare you, the user, of the pain of having to parse a
bunch of raw text for whatever cool new project you're working on.

This doesn't use any fancy parsing algorithms; it's basically just a
cargo-culted recursive descent parser.

**disclaimer:** this doesn't work yet, don't use it

---
* TODO: Replace UIOP stuff 
