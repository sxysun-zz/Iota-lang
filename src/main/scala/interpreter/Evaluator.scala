package main.scala.interpreter

import main.scala.core._

case class Evaluator (prog: Expression) {
  
  def mainEval(env: Environment): Any = transformAtom(eval(prog, env))
  
  import dfaState._
  def eval(exp: Expression, env: Environment): Atom = exp match {
    case AtomExpression(atom) => atom
    /**
     * depend on well-typed expression for l and r
     */
    case BinaryOperatorExpression(op, l, r) => {
      val lGot = eval(l, env)
      val rGot = eval(r, env)
      val temp = op.asInstanceOf[(Atom, Atom) => Atom](lGot, rGot)
      println(temp)
      temp
      /*
      lGot match {
        case AtomInt(lv) => rGot match {
          case AtomInt(rv) => AtomInt(op.asInstanceOf[(Int, Int) => Int](lv, rv))
          case AtomDouble(rv) => AtomDouble(op.asInstanceOf[(Int, Double) => Double](lv, rv))
          case _ => throw new RuntimeException("ill typed expression")
        }
        case AtomDouble(lv) => rGot match {
          case AtomInt(rv) => AtomDouble(op.asInstanceOf[(Double, Int) => Double](lv, rv))
          case AtomDouble(rv) => AtomDouble(op.asInstanceOf[(Double, Double) => Double](lv, rv))
          case _ => throw new RuntimeException("ill typed expression")
        }
        case _ => {
          throw new RuntimeException("")
        }
      }
      * 
      */
    }
    case _ => {
      throw new RuntimeException("no match for valid expression in evaluator")
    }
  }
  
  def transformAtom(a: Atom) = a match {
    case AtomInt(i) => i
    case AtomDouble(d) => d
    case AtomBoolean(b) => b
    case AtomString(s) => s
  }
}