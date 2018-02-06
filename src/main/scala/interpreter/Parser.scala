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
   * @return a cleaned version of abstract syntax tree
   */
  def getSExpression(): Expression = {
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
        case AtomicNode(Token(ARITHOPERATOR, content, prop), _) => {
           val opType = ast.checkType
           val left = get(cs.drop(1).head)
           val right = get(cs.drop(2).head)
           parseArithmeticOp(content, left, right, opType)
        }
        
        //boolean binary operator expression
        case AtomicNode(Token(BOOLOPERATOR, content, prop), _) => {
          if(cs.length != 4) throw new RuntimeException("wrong number of arguments in boolean " + 
              s"operation, location in $p, current argument is $cs")
          val left = get(cs.drop(1).head)
          val right = get(cs.drop(2).head)
          //parseBooleanOp(content, left, right)
//---------------------------------------------------------------------------------------------
          throw new RuntimeException("you entered boolean algebra, which is not supported yet")
        }
        
        //closure: defined function application expression
        case AtomicNode(Token(IDENTIFIER, content, prop), _) => {
          if(cs.length != 3) throw new RuntimeException("wrong number of arguments in defined closure function " + 
              s"application, location in $p, current argument is $cs")
          val funcName = content
          val body = get(cs.drop(1).head)
          //ClosureApplicationExpression(Closure(lookUp(funcName), body)
//---------------------------------------------------------------------------------------------
          throw new RuntimeException("you entered closure application, which is not supported yet")
        }
        
        //real-time function application expression
        case FragileNode(value, content, prop) => {
          if(cs.length != 3) throw new RuntimeException("wrong number of arguments in function " + 
              s"application, location in $p, current argument is $cs")
          val func = get(cs.head)
          val body = get(cs.drop(1).head)
//--------------------------------------------------------------------------------------------
//type check function usage implementation
          if(func.getClass.getName.equals(LambdaExpression)) throw new RuntimeException(func.getClass + "typed expression" + 
              "appeared in function application position, location in $p, current argument is $cs")
          ApplicationExpression(func, body)
        }
        
        //if expression
        case AtomicNode(Token(IF, content, prop), _) => {
          if(cs.length != 5) throw new RuntimeException("wrong number of arguments in if " + 
              s"expression, location in $p, current argument is $cs")
          val premises = get(cs.drop(1).head)
          val trueJump = get(cs.drop(2).head)
          val falseJump = get(cs.drop(3).head)
          IfExpression(premises, trueJump, falseJump)
        }
        
        //error expression
        case _ => {
          throw new RuntimeException("ill formed expression in parser")
        }
      }
      //atomic expression
      case AtomicNode(v, p) => v match {
        //case Token(IDENTIFIER, content, prop) => {AtomExpression()}
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
  
  import langType._
  private def parseArithmeticOp(op: String, left: Expression, right: Expression, opType: langType): Expression = op match {
    case "+" => {
      if(opType == int) BinaryOperatorExpression(Operations.iiplus, left, right)
      else BinaryOperatorExpression(Operations.ddplus, left, right)
    }
    case "-" => {
      if(opType == int) BinaryOperatorExpression(Operations.iiminus, left, right)
      else BinaryOperatorExpression(Operations.ddminus, left, right)
    }
    case "*" => {
      if(opType == int) BinaryOperatorExpression(Operations.iimulti, left, right)
      else BinaryOperatorExpression(Operations.ddmulti, left, right)
    }
    case "/" => {
      if(opType == int) BinaryOperatorExpression(Operations.iidivide, left, right)
      else BinaryOperatorExpression(Operations.dddivide, left, right)
    }
    case "%" => {
      if(opType == int) BinaryOperatorExpression(Operations.iimod, left, right)
      else BinaryOperatorExpression(Operations.ddmod, left, right)
    }
    case _ => throw new RuntimeException(s"no match for operator $op")
  }
  
  /*
   * @return the printed string of parser error
   */
  //private def parserError(position: MetaData, argument: AST): Expression = {
    
  //}
}