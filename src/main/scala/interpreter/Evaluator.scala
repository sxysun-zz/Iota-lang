package main.scala.interpreter

import main.scala.core._

case class Evaluator (prog: Expression) {
  
  def mainEval(env: Environment): Any = eval(prog, env).stripValue
  
  import dfaState._
  import langType._
  def eval (exp: Expression, env: Environment): Atom = exp match {
    case AtomExpression(atom) => atom
    case BinaryOperatorExpression(op, l, r) => {
      val lEvaled = eval(l, env)
      val rEvaled = eval(r, env)
      // handle the arithmetic operator case
      if(exp.inferType == double) op(lEvaled.arithSafeConversion, 
          rEvaled.arithSafeConversion).asInstanceOf[Atom]
      else op(lEvaled, rEvaled).asInstanceOf[Atom]
    }
    case _ => {
      throw new RuntimeException("no match for valid expression in evaluator")
    }
  }
}