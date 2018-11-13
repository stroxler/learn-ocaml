# An arithmetic machine - three implementations

This project has a tiny library with code from a running example in
Graham Hutton's "Programming in Haskell", translated to ocaml.

We define a small arithmetic Ast with binary operations for addition
and multiplication, then derive three run functions:
 - `simple_run` walks the ast and evaluates each subexpression, in much
   the way that a typical lisp-in-lisp evaluator would. It's really just a
   fold over the AST; since our AST is so simple (literals and binary
   operators) there's almost no code.
 - `cps_run` performs a simple CPS transform, in a way where we make the
   evaluation order left-to-right very explicit and maintain a stack of
   continuations representing the rest of the program. Because a
   binary-operator-only minilanguage is so simple, the CPS is pretty
   straightforward and doesn't use the same patterns we would need for a
   more serious runner, but I added comments with more details.
 - `stack_run` has an interpreter and a compiler; it first walks the AST
   and produces a list of stack operations, then it traverses the operations
   in a stack evaluator. Again our binary language is so simple that this is
   far easier than it would be for a more complete language: in particular,
   control operators and functions would require some form of jump instruction
   that would also prevent us from using a simple list traversal.

In spite of the simplicity of our language, I do think that the exercise
is pretty instructional. I'm hoping to revisit this code and add a parser
to it later.
