package main.scala.interpreter

import main.scala.core._

case class Evaluator (prog: List[Expression]) {
  
  def mainEval(env: Environment): List[Any] = {
    prog.map(eval(_, env).stripValue)
  }
  
  import dfaState._
  import langType._
  /**
   * `WARNING` it is the evaluator during runtime that is responsible for maintaining a symbol table
   */
  def eval (exp: Expression, env: Environment): Atom = exp match {
    case AtomExpression(atom) => atom match {
      case AtomIdentifier(name) => env.lookUp(name) match {
        case Right(x) => x
        case Left(x) => throw new RuntimeException(x + s"at $exp")
      }
      case _ => atom
    }
    case BinaryOperatorExpression(op, l, r) => {
      val lEvaled = eval(l, env)
      val rEvaled = eval(r, env)
      //println(l)
      //println(lEvaled.inferType(env))
      //println(exp.inferType(env))
      
      // handle the arithmetic operator case
      if(exp.inferType(env) == double) op(lEvaled.arithSafeConversion, 
          rEvaled.arithSafeConversion).asInstanceOf[Atom]
      else op(lEvaled, rEvaled).asInstanceOf[Atom]
    }
    case DefineExpression(n, v) => {
      val definedExpr = eval(v, env)
      env.extendEnvironment(n, definedExpr)
      AtomIdentifier(n)
    }
    case ApplicationExpression(f, b) => {
      f match {
        case LambdaExpression(v_, b_) => {
          val applicatee = eval(b, env)
          val newEnv = Environment().copyExternal(env)extendEnvironment(v_, applicatee)
          eval(b_, newEnv)
        }
        case _ => throw new RuntimeException(s"not function application in $f")
      }
    }
    case exp@LambdaExpression(v, b) => {
      eval(b, env)
    }
    case _ => {
      throw new RuntimeException("no match for valid expression in evaluator")
    }
  }
}