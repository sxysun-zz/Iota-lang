package main.scala.core

/**
 * `IMPORTANT`
 * there are six basic expressions in iota-lang
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
   * `WARNING` no identifier support yet
   * @return the inferred type of a certain expression
   */
  def inferType(env: Environment): langType = {
    def infer(exp: Expression): langType = exp match {
      case LambdaExpression(v, b) => lambda
      case DefineExpression(v, v_) => unit
      case AtomExpression(v) => v match {
        case AtomBoolean(_) => boolean
        case AtomDouble(_) => double
        case AtomInt(_) => int
        case AtomString(_) => string
        case t@AtomIdentifier(_) => t.inferType(env)
      }
      //Closure property of Operations --------- subject to change 
      case BinaryOperatorExpression(op, l, r) => {
        val lInferred = infer(l)
        val rInferred = infer(r)
        if(lInferred == double || rInferred == double) double
        else lInferred
      }
      case UnclosedOperationExpression(_, _ ,_) => boolean
      //----------------this rule of inference is incorrect------------------------------
      case ApplicationExpression(f, b) => infer(f)
      case IfExpression(ar, f, s) => infer(f)
      //----------------identifier support------------------------------
      case _ => throw new RuntimeException("failed to infer type of " + exp.print())
    }
    infer(this)
  }
}

case class LambdaExpression (varName: String, body: Expression) extends Expression

case class DefineExpression (varName: String, value: Expression) extends Expression

case class AtomExpression (value: Atom) extends Expression

case class BinaryOperatorExpression [A <% Atom](operator: Function2[A, A, A], left: Expression, right: Expression) extends Expression

case class UnclosedOperationExpression (operator: Function2[AtomDouble, AtomDouble, AtomBoolean], left: Expression, right: Expression) extends Expression

case class ApplicationExpression (functionName: Expression, body: Expression) extends Expression

case class IfExpression (argument: Expression, first: Expression, second: Expression) extends Expression

/**
 * @return a function closure to make lexical scope for variables, use for pre-defined functions
 */
case class Closure (func: Expression, env: Environment) extends Expression

object langType extends Enumeration {
  type langType = Value
  val lambda, double, int, string, boolean, unit = Value
}
/*
sealed trait Operation[A]

case class AddOperation[A](f: (A, A) => A) extends Operation[A]
*/