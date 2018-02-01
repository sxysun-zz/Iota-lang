# Alayi-lang
### introduction: 
Lisp dialet written with Scala, light weight, equipped with latex like grammar for unicode operators. this language is typed. Provides helpful and detailed debug information. AST vadility check.

### features:
* Lambda calculus application
* Lexical Scope
* environment model
* auto type inference
* code vadility check

### sample:
``` scala
(\lambda (y) ((\lambda (x) (y (x x))) (\lambda (x) (y (x x)))))
(\equiv (+ x 1) (* 3.4 2))
```
