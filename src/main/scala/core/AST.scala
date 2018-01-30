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
  
}
case class AtomicNode(value: Token, parent: AST) extends AST

case class FragileNode (value: Token, child: ListBuffer[AST], parent: FragileNode) extends AST
case class FragileRoot (child: FragileNode) extends AST

/*
case class Root (child: Node) extends AST
case class Node (value: Token, child: List[AST], parent: AST) extends AST
*/