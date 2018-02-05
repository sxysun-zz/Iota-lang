# iota-lang
### introduction: 
Lisp dialet written with Scala, light weight, equipped with latex-ish grammar for unicode operators. this language is typed. Provides helpful and detailed debug information. code vadility check.

### features:
* Lambda[ iota ] calculus application
* Lexical Scope
* environment model (functional datastructure for symbol tables)
* auto type inference (Martin Lof)
* code vadility check (AST reform)

### sample:
``` scala
(\iota (y) ((\iota (x) (y (x x))) (\iota (x) (y (x x)))))
(= x 6.8)
(\equiv (+ x 1) (* 3.4 2))
(if 真 (% x 2) (^ x 3))
```
