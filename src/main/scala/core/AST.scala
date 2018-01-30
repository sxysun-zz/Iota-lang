package main.scala.core

import main.scala.core._

sealed trait AST 
case class AtomicNode(value: Token, parent: AST) extends AST
case class Node (value: Token, child: List[Node], parent: AST) extends AST
case class Root (child: List[Node]) extends AST