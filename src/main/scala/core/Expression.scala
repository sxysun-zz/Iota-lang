package main.scala.core

/**
 * `IMPORTANT`
 * there are six basic expressions in alayi-lang
 * 1. define expression like (= x 1)
 * (= iden exp)
 * 2. lambda expression like (λ (x) (* x x))
 * (λ (iden) exp)
 * 3. binary operator expression like (+ 1 3)
 * (op exp exp)
 * 4. function application expression like ((λ (x) (* x x)) (+ 1 3))
 * (exp exp)
 * 5. if expression like (if (#t) (+ x 1) (- x 2))
 * (if exp exp exp)
 * 6. atomic expression like #f, 1, "string", 1.7
 */
sealed trait Expression {
  
  /**
   * fail to build if change this function to override 
   */
  def print(): String = {
    def toStringRec(exp: Expression): String = exp match {
      case LambdaExpression(v, b) => s"(lambda ($v) >( " + toStringRec(b) + " ) )"
      case DefineExpression(v, v_) => s"(define $v >( " + toStringRec(v_) + " ) )"
      case AtomExpression(v) => v.toString()
      case BinaryOperatorExpression(op, l, r) => s"( op >( " + toStringRec(l) + " ) >( " + toStringRec(r) + " ) )"
      case ApplicationExpression(f, b) => "( >( " + toStringRec(f) + " ) >( " + toStringRec(b) + " ) )"
      case IfExpression(ar, f, s) => "( if >( " + toStringRec(ar) + " ) >( " + toStringRec(f) + 
        " ) >( " + toStringRec(s) + " ) "
      case _ => "undefined toString method"
    }
    toStringRec(this)
  }
  
  import langType._
  /**
   * ```IMPORTANT``` this function assumes well-typed expressions to work ideally, 
   * because the type checking has been done at the abstract syntax tree to 
   * expression stage in parser
   * @return the inferred type of a certain expression
   */
  def inferType: langType = {
    def infer(exp: Expression): langType = exp match {
      case LambdaExpression(v, b) => lambda
      case DefineExpression(v, v_) => unit
      case AtomExpression(v) => v match {
        case AtomBoolean(_) => boolean
        case AtomDouble(_) => double
        case AtomInt(_) => int
        case AtomString(_) => string
      }
      //Closure property of Operations --------- subject to change 
      case BinaryOperatorExpression(op, l, r) => {
        val lInferred = infer(l)
        val rInferred = infer(r)
        if(lInferred == double || rInferred == double) double
        else lInferred
      }
      //----------------this rule of inference is incorrect------------------------------
      case ApplicationExpression(f, b) => infer(b)
      case IfExpression(ar, f, s) => infer(f)
      //----------------identifier support------------------------------
      case _ => throw new RuntimeException("failed to infer type of " + exp.print())
    }
    infer(this)
  }
  /*
   * (define infer
  (lambda (exp env)
    (match exp
      ((? symbol? x)
       (let ((sed (search x env)))
         (cond
           ((not sed)
            (display "undefined var"))
           (else sed))))
      ((? number? x) `int)
      ((? string? x) `string)
      ((? boolean? x) `boolean)
      (`(lambda (,x) ,e)
       (closure exp env))
      (`(`let ((,x ,e1)) ,e2)
       (let ((t (infer e1 env)))
         (infer e2 (ext-env x t env))))
      (`(if ,t ,e1 ,e2)
       (let ((b (infer t env)))
         (cond
           ((eq? b `bool)
            (let ((t1 (infer e1 env))
                  (t2 (infer e2 env)))
              (cond
                ((eq? t1 t2) t1)
                (else
                 (display "branch type mismatch"))))))))
      (`(,e1 ,e2)
       (let ((t1 (infer e1 env))
             (t2 (infer e2 env)))
         (match t1
           ((closure `(lambda (,x) ,e) env-old)
            (infer e (ext-env x t2 env-old))))))
      (`(,e1 ,op ,e2)
       (let ((t1 (infer e1 env))
             (t2 (infer e2 env)))
         (cond
           ((not (eq? t1 `int)) (display "first operand not int"))
           ((not (eq? t2 `int)) (display "second operand not int"))
(else `int)))))))
   */
}

case class LambdaExpression (varName: String, body: Expression) extends Expression

case class DefineExpression (varName: String, value: Expression) extends Expression

case class AtomExpression (value: Atom) extends Expression

case class BinaryOperatorExpression [A <% Atom](operator: (A, A) => A, left: Expression, right: Expression) extends Expression

case class ClosureApplicationExpression (closure: Closure, body: Expression) extends Expression

case class ApplicationExpression (functionName: Expression, body: Expression) extends Expression

case class IfExpression (argument: Expression, first: Expression, second: Expression) extends Expression

/**
 * @return a function closure to make lexical scope for variables, use for pre-defined functions
 */
case class Closure (func: LambdaExpression, env: Environment)

/**
 * @return a closure strucutre of certain identifier
 */
case class AtomClosure (value: AtomIdentifier, env: Environment)

object langType extends Enumeration {
  type langType = Value
  val lambda, double, int, string, boolean, unit = Value
}
/*
sealed trait Operation[A]

case class AddOperation[A](f: (A, A) => A) extends Operation[A]
*/