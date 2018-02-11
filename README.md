# iota-lang
### introduction: 
Lisp dialet written with Scala, light weight, equipped with latex-ish grammar for unicode operators. this language is typed. Provides helpful and detailed debug information. code vadility check. (I used to call this repo alayi-lang)
``` racket
(= fac
   (\lambda (x)
     (if (\eq (- x 1) 0)
         1
         (* x (fac (- x 1))))))
(= x 9)
(fac x)
```

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
* higher order function and currying
2. Types
* auto type inference (Martin Lof method)
* lexical scope
* environment model (purely functional datastructure for symbol tables)
3. Debugging
* code vadility check (AST reform)
* useful and accurate exception information
4. Interactive REPL
* evaluate the expression you want at anytime with friendly environment maintaince
``` racket
(= x 1.0)
(= f (\lambda (x) (+ x x)))
\lookup
(f x)
(f res2)
(f res3)
(if (\eq x 1.0) (= x (+ x 1)) (= x 2.1))
(+ x 0)
(= b #f)
(\equiv b #t)
exit
```

### sample:
``` racket
(λ (y) ((λ (x) (y (x x))) (λ (x) (y (x x)))))
(= x 6.8)
(≡ (+ x 1) (* 3.4 2))
(if #t (% x 2) (^ x 3))
```
