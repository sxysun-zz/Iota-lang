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
  private def getRawAST(): FragileRoot = {
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
   * @return no interruption if the type is right, a warning if type is wrong
   */
  private def typeCheck() = {
    
  }
  
  /**
   * @return a cleaned version of abstract syntax tree
   */
  def getAST(): Expression = {
    import dfaState._
    root = getRawAST()
    def get(ast: AST): Expression = ast match {
      case FragileRoot(c) => get(c)
      case FragileNode(v, cs, p) => cs.head match{
        
        //take care of all procedures and lambda expressions
        
        //lambda expression
        case AtomicNode(Token(LAMBDA, content, prop), _) => {
          if(cs.length != 6) throw new RuntimeException("wrong number of arguments in lambda" + 
              s"expression, location in $p, current argument is $cs")
          var varName: String = ""
          cs.drop(2).head match {
            case AtomicNode(Token(IDENTIFIER, content, prop), _) => varName = content
            case _ => throw new RuntimeException("wrong lambda expression identifier type" + 
                s" at $p, current argument is $cs")
          }
          LambdaExpression(varName, get(cs.drop(4).head))
        }
        
        //define expression
        case AtomicNode(Token(DEFINE, content, prop), _) => {
          if(cs.length != 4) throw new RuntimeException("wrong number of arguments in define " + 
              s"clause, location in $p, current argument is $cs")
          var varName: String = ""
          cs.drop(1).head match {
            case AtomicNode(Token(IDENTIFIER, content, prop), _) => varName = content
            case _ => throw new RuntimeException("wrong define clause variable name" + 
                s" at $p, current argument is $cs")
          }
          DefineExpression(varName, get(cs.drop(2).head))
        }
        
        //arithmetic binary operator expression
        case AtomicNode(Token(ARITHOPERATOR, content, prop), _) => content match {
            case "+" => {
              val left = get(cs.drop(1).head)
              val right = get(cs.drop(2).head)
              left match {
                case AtomExpression(AtomInt(_)) => BinaryOperatorExpression((x:Int, xs:Int) => x+xs, left, right)
                case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression((x:Double, xs:Double) => x+xs, left, right)
                case _ => throw new RuntimeException("invalid type for + operator")
              }
            }
            case "-" => {
              val left = get(cs.drop(1).head)
              val right = get(cs.drop(2).head)
              left match {
                case AtomExpression(AtomInt(_)) => BinaryOperatorExpression((x:Int, xs:Int) => x-xs, left, right)
                case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression((x:Double, xs:Double) => x-xs, left, right)
                case _ => throw new RuntimeException("invalid type for - operator")
              }
            }
            case "*" => {
              val left = get(cs.drop(1).head)
              val right = get(cs.drop(2).head)
              left match {
                case AtomExpression(AtomInt(_)) => BinaryOperatorExpression((x:Int, xs:Int) => x*xs, left, right)
                case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression((x:Double, xs:Double) => x*xs, left, right)
                case _ => throw new RuntimeException("invalid type for * operator")
              }
            }
            case "/" => {
              val left = get(cs.drop(1).head)
              val right = get(cs.drop(2).head)
              left match {
                case AtomExpression(AtomInt(_)) => BinaryOperatorExpression((x:Int, xs:Int) => x/xs, left, right)
                case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression((x:Double, xs:Double) => x/xs, left, right)
                case _ => throw new RuntimeException("invalid type for * operator")
              }
            }
            case "%" => {
              val left = get(cs.drop(1).head)
              val right = get(cs.drop(2).head)
              left match {
                case AtomExpression(AtomInt(_)) => BinaryOperatorExpression((x:Int, xs:Int) => x%xs, left, right)
                case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression((x:Double, xs:Double) => x%xs, left, right)
                case _ => throw new RuntimeException("invalid type for * operator")
              }
            }
            case _ => {
              throw new RuntimeException("unrecognized arithmetic operator")
            }
        }
        
        //boolean binary operator expression
        case AtomicNode(Token(BOOLOPERATOR, content, prop), _) => {
          throw new RuntimeException("")
        }
        
        //function application expression
        case FragileNode(value, content, prop) => {
          throw new RuntimeException("")
        }
        
        //if expression
        case AtomicNode(Token(IF, content, prop), _) => {
          throw new RuntimeException("")
        }
        
        //error expression
        case _ => {
          throw new RuntimeException("")
        }
      }
      //atomic expression
      case AtomicNode(v, p) => v match {
        case Token(INT, content, prop) => {AtomExpression(AtomInt(content.toInt))}
        case Token(DOUBLE, content, prop) => {AtomExpression(AtomDouble(content.toDouble))}
        case Token(STRING, content, prop) => {AtomExpression(AtomString(content))}
        case Token(BOOLEAN, content, prop) => {AtomExpression(AtomBoolean(content.toBoolean))}
        case Token(k, content, prop) => {throw new RuntimeException("wrong atomic node" +
	        s"in syntax tree at $prop, token is $content, token type is $k")
        }
      }
    }
    get(root)
  }
  
  /**
   * @return the printed string of parser error
   */
  private def parserError(position: MetaData, argument: AST) = {
    
  }
}