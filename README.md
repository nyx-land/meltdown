# Meltdown - A Common Lisp markdown parser
> The story goes like this...

This is just a Common Lisp parser for markdown. It doesn't use any fancy parsing
algorithms, but its goal is to return the AST as standard objects for a markdown
document so that you can do whatever you want with it. It has no external
dependencies[^1]

This doesn't use any fancy parsing algorithms; it's basically just a
cargo-culted recursive descent parser.

# Why

The other markdown parsers that currently exist for CL assume that the user
wants to convert the markdown to HTML. This system makes no such assumptions.

**disclaimer:** very work-in-progress and not useful yet

---
[^1]: TODO: Replace UIOP stuff 
