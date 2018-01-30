package main.scala.interpreter

import scala.annotation.Annotation
import main.scala.core._

case class Parser (tokens: List[Token]) {
  
  private var root: Root = Root(List())
  private var currentNode = root
  
  /**
   * `WARNING` not guaranteed to be a right syntax tree
   * @return a crude crafted abstract syntax tree
   */
  def getAST() = {
    import dfaState._
    //@annotation.tailrec
    def getTail(l: List[Token]) = l.head match {
      case t@Token(LPAREN, _, _) => {
        val nn = Node(t, List())
        currentNode = nn
        currentNode.child:+nn
        
      }
      case 
    }
    getTail(List())
  }
}