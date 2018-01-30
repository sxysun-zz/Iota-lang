package main.scala.interpreter

import main.scala.core._

case class Evaluator (ast: AST) {
  def eval(ast: AST): Any = ast match {
    case FragileRoot(c) => eval(c)
    case AtomicNode(v, p) => {
      
    }
    case FragileNode(v, cs, p) => {
      
    }
  }
}