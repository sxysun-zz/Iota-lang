package main.scala.interpreter

import main.scala.core._

case class Evaluator (ast: AST) {
  import dfaState._
  /*
  def eval(ast: AST, env: Environment): Atom = ast match {
    case FragileRoot(c) => eval(c, env)
    case AtomicNode(v, p) => v match {
      case Token(INT, v, _) => LiteralInt(v.toInt)
      case Token(DOUBLE, v, _) => LiteralDouble(v.toDouble)
      case Token(STRING, v, _) => LiteralString(v)
      case Token(BOOLEAN, v, _) => LiteralBoolean(v.toBoolean)
      case Token(IDENTIFIER, v, _) => env.lookUp(v) //look up environment
      case _ => {
        throw new RuntimeException("unmatched satomic type")
      }
    }
    case FragileNode(v, cs, p) => cs.head match{
      case AtomicNode(Token(BINOPERATOR, v, cp), _) => v match {
        case "+" => eval(cs.drop(1).head, env) + eval(cs.drop(2).head, env)
        case "*" => eval(cs.drop(1).head, env) * eval(cs.drop(2).head, env)
        case "/" => eval(cs.drop(1).head, env) / eval(cs.drop(2).head, env)
        case "-" => eval(cs.drop(1).head, env) - eval(cs.drop(2).head, env)
        case "%" => eval(cs.drop(1).head, env) % eval(cs.drop(2).head, env)
        case _ => throw new RuntimeException("No match for binary operator" + cp)
      }
      case _ => {
        println("here")
      }
    }
  }
  * 
  */
}