# iota-lang
### introduction: 
Lisp dialet written with Scala, light weight, equipped with latex like grammar for unicode operators. this language is typed. Provides helpful and detailed debug information. AST vadility check.

### features:
* Lambda[ iota ] calculus application
* Lexical Scope
* environment model
* auto type inference
* code vadility check

### sample:
``` scala
(\iota (y) ((\iota (x) (y (x x))) (\iota (x) (y (x x)))))
(= x 6.8)
(\equiv (+ x 1) (* 3.4 2))
```
