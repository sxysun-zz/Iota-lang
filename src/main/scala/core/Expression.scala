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
sealed trait Expression

case class LambdaExpression (varName: String, body: Expression) extends Expression

case class DefineExpression (varName: String, value: Expression) extends Expression

case class AtomExpression (value: Atom) extends Expression

case class BinaryOperatorExpression [B <% Atom, C <% Atom, A <% Atom](operator: (A, B) => C, left: Expression, right: Expression) extends Expression

case class ClosureApplicationExpression (closure: Closure, body: Expression) extends Expression

case class ApplicationExpression (functionName: Expression, body: Expression) extends Expression

case class IfExpression (argument: Expression, first: Expression, second: Expression) extends Expression

/**
 * @return a function closure to make lexical scope for variables, use for pre-defined functions
 */
case class Closure (func: LambdaExpression, env: Environment)

object langType extends Enumeration {
  type langType = Value
  val lambda, double, int, string, boolean, unit = Value
}
/*
sealed trait Operation[A]

case class AddOperation[A](f: (A, A) => A) extends Operation[A]
*/