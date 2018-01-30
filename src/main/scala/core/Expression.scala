package main.scala.core

/**
 * all kinds of expressions used in `eval`
 */
sealed trait Expression

case class DefineExpression (varName: String, value: Expression) extends Expression

case class LambdaExpression (varName: String, body: Expression) extends Expression

case class AtomExpression (value: Atom) extends Expression

case class BinaryOperatorExpression [A, B] (operator: (A, A) => B) extends Expression

case class ApplicationExpression (functionName: String, body: Expression) extends Expression

case class IfExpression (argument: Expression, first: Expression, second: Expression) extends Expression