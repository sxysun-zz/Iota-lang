package main.scala.interpreter

import main.scala.core._

case class Evaluator (prog: List[Expression]) {
  
  def mainEval(env: Environment): List[Any] = {
    val ret = prog.map(eval(_, env).stripValue)
    ret
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
      if(exp.inferType(env) == double) op(lEvaled.arithSafeConversion, 
          rEvaled.arithSafeConversion).asInstanceOf[Atom]
      else op(lEvaled, rEvaled).asInstanceOf[Atom]
    }
    case UnclosedOperationExpression(op, l, r) => {
      val lEvaled = eval(l, env)
      val rEvaled = eval(r, env)
      if(lEvaled.inferType(env) == double && rEvaled.inferType(env) == double) 
        op(lEvaled.asInstanceOf[AtomDouble], 
          rEvaled.asInstanceOf[AtomDouble]).asInstanceOf[Atom]
      else throw new RuntimeException(s"ill formed expression in $l and $r")
    }
    case DefineExpression(n, v) => v match {
      case lam@LambdaExpression(v_, b_) => {
        env.functionTable = env.functionTable.updated(n, lam)
        AtomIdentifier(n)
      }
      case _ => {
        val definedExpr = eval(v, env)
        env.extendEnvironment(n, definedExpr)
        AtomIdentifier(n)
      }
    }
    case ApplicationExpression(f, b) => {
      //println(exp)
      //println(env.functionTable.size)
      f match {
        case AtomExpression(AtomIdentifier(funcN)) => {
          val func = env.functionTable(funcN)
          eval(ApplicationExpression(func, b), env)
        }
        case LambdaExpression(v_, b_) => {
          val applicatee = eval(b, env)
// entrance of higher order function -------------------------------------------------
          val newEnv = Environment().copyExternal(env).extendEnvironment(v_, applicatee)
          eval(b_, newEnv)
        }
        case exp@_ => {
          eval(exp, env) match {
            case exp2@AtomLambda(v_, b_) => {
              eval(ApplicationExpression(exp2.toLambdaExpression, b), env)
            }
            case _ => throw new RuntimeException(s"not function application in $f")
          }
        }
      }
    }
    case IfExpression(p, t, f) => {
      val prem = eval(p, env)
      prem match {
        case AtomBoolean(premises) => {
          if(premises) eval(t, env)
          else eval(f, env)
        }
        case _ => throw new RuntimeException(s"the premises condition is not a boolean in if statement: $p")
      }
    }
    case exp@LambdaExpression(v, b) => {
      AtomLambda(v, b)
    }
    case exp@_ => {
      throw new RuntimeException(s"no match for valid expression in evaluator: $exp")
    }
  }
}