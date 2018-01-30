package main.scala.interpreter

import scala.annotation.Annotation
import main.scala.core._
import scala.collection.mutable._

case class Parser (tokens: List[Token]) {
  
  private var root: FragileRoot = FragileRoot(FragileNode(tokens.head, ListBuffer(), null))
  private var currentNode = root.child
  
  /**
   * `WARNING` not guaranteed to be a right syntax tree
   * `IMPORTANT` this function has side effect
   * @return a crude crafted abstract syntax tree
   */
  def getAST(): FragileRoot = {
    import dfaState._
    if(currentNode.value.kind != LPAREN) throw new RuntimeException("syntax error at file head")
    @annotation.tailrec
    def getTail(l: List[Token]): Unit = l.head match {
      case t@Token(END, _,_) => {Unit} // termination command
      case t@Token(LPAREN, _, _) => {
        val newNode = FragileNode(t, ListBuffer(), currentNode)
        currentNode.child.append(newNode)
        currentNode = newNode
        getTail(l.drop(1))
      }
      case t@Token(RPAREN, _, _) => {
        //println(l map (_.value))
        val newNode = AtomicNode(t, currentNode)
        currentNode.child.append(newNode)
        currentNode = currentNode.parent
        getTail(l.drop(1))
      }
      case t@_ => {
        currentNode.child.append(AtomicNode(t, currentNode))
        getTail(l.drop(1))
      }
    }
    getTail(tokens.drop(1))
    root
  }
  
  /**
   * @return an immutable AST
   */
  private def solidifyAST() = {
    /*
    def get(tr: AST): Unit = tr match {
      case t@FragileNode(x,y,z) => {
        val newN = Node(x, y.toList, z)
        t = newN
        newN.child.map(get(_))
      }
      
    }
    get(root.child)
    * 
    */
  }
}