# iota-lang
### introduction: 
Lisp dialet written with Scala, light weight, equipped with latex-ish grammar for unicode operators. this language is typed. Provides helpful and detailed debug information. code vadility check. (I used to call this repo alayi-lang)

### features:
1. Lambda calculus application
* raw operation on perocedures (examples include church enumeration :
``` racket
(= zero (λ (f) (λ (x) (x))))
(= (add-one n)
  (λ (f) (λ (x) (f (((n) (f)) (x))))))
```
and useful lambda calculus rules like beta reduction and alpha conversion to create data structure:
``` racket
(= (consn x y)
  (λ (m) (m x y)))
(= (carn x)
  (x (λ (a b) a)))
(= (cdrn x)
  (x (λ (a b) b)))
```
2. Types
* auto type inference (Martin Lof method)
* lexical scope
* environment model (purely functional datastructure for symbol tables)
3. Debugging
* code vadility check (AST reform)
* useful and accurate exception information

### sample:
``` racket
(λ (y) ((λ (x) (y (x x))) (λ (x) (y (x x)))))
(= x 6.8)
(≡ (+ x 1) (* 3.4 2))
(if 真 (% x 2) (^ x 3))
```
