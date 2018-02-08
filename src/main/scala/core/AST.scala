package main.scala.core

import main.scala.core._
import scala.collection.mutable._

sealed trait AST {
  def size = {
    def getSize(c: AST): Int = c match {
      case AtomicNode(_,_) => 1
      case FragileRoot(cs) => (1 /: cs) ((x, xs) => x + getSize(xs))
      case FragileNode(_, cs, _) => (1 /: cs.map(getSize(_))) (_+_)
    }
    getSize(this)
  }
  
  import langType._
  import dfaState._
  
  /**
   * @return type of current abstract syntax tree
   */
  def checkType(env: TypeEnvironment): langType = {
    typeCheck(this, env)
  }
  
  /**
   * `WARNING` this will change the environment table
   * this will not identify any of the function applications
   * @return no interruption if the type is right, a warning if type is wrong
   */
  private def typeCheck(exp: AST, env: TypeEnvironment): langType = exp match {
    case FragileRoot(cs) => {cs.map(typeCheck(_, env));unit}
    case FragileNode(v, cs, p) => cs.head match {
      
        //lambda expression
        case AtomicNode(Token(LAMBDA, content, prop), _) => lambda
        
        //define expression
        case AtomicNode(Token(DEFINE, content, prop), _) => {
          var varName = ""
          cs.drop(1).head match {
            case AtomicNode(Token(IDENTIFIER, _content, _prop), _) => varName = _content
            case _ => throw new RuntimeException("type check failed, wrong define clause variable name" + 
                s" at $p, current argument is $cs")
          }
          val valType = typeCheck(cs.drop(2).head, env)
          env.extendEnvironment(varName, valType)
          unit
        }
        
        //arithmetic binary operator expression
        case AtomicNode(Token(ARITHOPERATOR, content, prop), _) => {
           if(cs.length != 4) throw new RuntimeException("wrong number of arguments in arithmetic " + 
              s"operation, location in $p, current argument is $cs")
           val left = typeCheck(cs.drop(1).head, env)
           val right = typeCheck(cs.drop(2).head, env)
           if((left != int && left != double) || (right != int && right != double)) {
             println(s"$left  $right")
             throw new RuntimeException(s"ill typed expression, expected arithmetic ones, at $prop")
           }
           if(left == double || right == double) double
           else int
        }
        
        //boolean binary operator expression
        case AtomicNode(Token(BOOLOPERATOR, content, prop), _) => boolean
        
        //closure: defined function application expression
        case AtomicNode(Token(IDENTIFIER, content, prop), _) => {
          if(cs.length != 3) throw new RuntimeException("wrong number of arguments in defined closure function " + 
              s"application, location in $p, current argument is $cs")
          val funcName = content
//--------not implemented the function procedure, the function should have return value in environment
          env.lookUp(funcName) match {
            case Right(x) => x
            case Left(x) => throw new RuntimeException(x)
          }
          //this will always return lambda
        }
        
        //real-time function application expression
        case FragileNode(value, content, prop) => {
          if(cs.length != 3) throw new RuntimeException("wrong number of arguments in function " + 
              s"application, location in $p, current argument is $cs")
          val func = typeCheck(cs.head, env)
          //val body = typeCheck(cs.drop(1).head, env.extendEnvironment(name, attribute))
//---------------not implemented-------------------
          //this will always return lambda
          func
        }
        
        //if expression
        case AtomicNode(Token(IF, content, prop), _) => {
          if(cs.length != 5) throw new RuntimeException("wrong number of arguments in if " + 
              s"expression, location in $p, current argument is $cs")
          val premises = typeCheck(cs.drop(1).head, env)
          val trueJump = typeCheck(cs.drop(2).head, env)
          val falseJump = typeCheck(cs.drop(3).head, env)
          if(premises != boolean) throw new RuntimeException("ill type in premises of if expression")
          if(trueJump != falseJump) throw new RuntimeException("indeterministic type if expression, no lub()")
//---------------need better implementation-------------------
          trueJump
        }
        
        //error expression
        case _ => {
          throw new RuntimeException("ill typed expression in parser")
        }
    }
    case AtomicNode(v, p) => v match {
        case Token(IDENTIFIER, content, prop) => env.lookUp(content) match {
          case Right(x) => x
          case Left(x) => throw new RuntimeException(x)
        }
        case Token(INT, content, prop) => int
        case Token(DOUBLE, content, prop) => double
        case Token(STRING, content, prop) => string
        case Token(BOOLEAN, content, prop) => boolean
        case Token(k, content, prop) => {throw new RuntimeException("ill type " +
            s"in syntax tree at $prop, token is $content, token type is $k")}
    }
  }
}
case class AtomicNode(value: Token, parent: AST) extends AST

case class FragileNode (value: Token, child: ListBuffer[AST], parent: FragileNode) extends AST

case class FragileRoot (children: ListBuffer[FragileNode]) extends AST

/*
case class Root (child: Node) extends AST
case class Node (value: Token, child: List[AST], parent: AST) extends AST
*/