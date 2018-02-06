package main.scala.core

import main.scala.core._
import scala.collection.mutable._

sealed trait AST {
  def size = {
    def getSize(c: AST): Int = c match {
      case AtomicNode(_,_) => 1
      case FragileRoot(c) => 1 + getSize(c)
      case FragileNode(_, cs, _) => (1 /: cs.map(getSize(_))) (_+_)
    }
    getSize(this)
  }
  
  import langType._
  import dfaState._
  
  /**
   * @return type of current abstract syntax tree
   */
  def checkType: langType = {
    typeCheck(this)
  }
  
  /**
   * @return no interruption if the type is right, a warning if type is wrong
   */
  private def typeCheck(exp: AST): langType = exp match {
    case FragileRoot(c) => typeCheck(c)
    case FragileNode(v, cs, p) => cs.head match {
      
        //lambda expression
        case AtomicNode(Token(LAMBDA, content, prop), _) => lambda
        
        //define expression
        case AtomicNode(Token(DEFINE, content, prop), _) => unit
        
        //arithmetic binary operator expression
        case AtomicNode(Token(ARITHOPERATOR, content, prop), _) => {
           if(cs.length != 4) throw new RuntimeException("wrong number of arguments in arithmetic " + 
              s"operation, location in $p, current argument is $cs")
           val left = typeCheck(cs.drop(1).head)
           val right = typeCheck(cs.drop(2).head)
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
          val body = typeCheck(cs.drop(1).head)
//---------------not implemented-------------------
          body
        }
        
        //real-time function application expression
        case FragileNode(value, content, prop) => {
          if(cs.length != 3) throw new RuntimeException("wrong number of arguments in function " + 
              s"application, location in $p, current argument is $cs")
          val func = typeCheck(cs.head)
          val body = typeCheck(cs.drop(1).head)
//---------------not implemented-------------------
          body
        }
        
        //if expression
        case AtomicNode(Token(IF, content, prop), _) => {
          if(cs.length != 5) throw new RuntimeException("wrong number of arguments in if " + 
              s"expression, location in $p, current argument is $cs")
          val premises = typeCheck(cs.drop(1).head)
          val trueJump = typeCheck(cs.drop(2).head)
          val falseJump = typeCheck(cs.drop(3).head)
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
        //case Token(IDENTIFIER, content, prop) => {AtomExpression()}
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

case class FragileRoot (child: FragileNode) extends AST

/*
case class Root (child: Node) extends AST
case class Node (value: Token, child: List[AST], parent: AST) extends AST
*/