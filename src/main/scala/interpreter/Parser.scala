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
  
  def typeInferenceTest() = typeCheck(getRawAST())
  
  /**
   * @return no interruption if the type is right, a warning if type is wrong
   */
  import langType._
  import dfaState._
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
          
          if(func.getClass.getName.equals("LambdaExpression")) throw new RuntimeException(func.getClass + "typed expression" + 
              "appeared in function application position, location in $p, current argument is $cs")
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
        case Token(k, content, prop) => {throw new RuntimeException("mis matched type" +
	        s"in syntax tree at $prop, token is $content, token type is $k")}
    }
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
          val leftType = typeCheck(cs.drop(1).head)
          val rightType = typeCheck(cs.drop(2).head)
           val left = get(cs.drop(1).head)
           val right = get(cs.drop(2).head)
           parseArithmeticOp(content, left, right)
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
  
  private def parseArithmeticOp(op: String, left: Expression, right: Expression): Expression = op match {
    case "+" => {
      println(s"$op for $left and $right")
      left match {
          case AtomExpression(AtomInt(_)) => right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.iiplus, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.idplus, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }    
          case AtomExpression(AtomDouble(_)) =>  right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.diplus, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.ddplus, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }
          case _ => throw new RuntimeException("invalid type for " + op + " operator")
      }
    }
    case "-" => {
      println(s"$op for $left and $right")
      left match {
          case AtomExpression(AtomInt(_)) => right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.iiminus, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.idminus, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }    
          case AtomExpression(AtomDouble(_)) =>  right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.diminus, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.ddminus, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }
          case _ => throw new RuntimeException("invalid type for " + op + " operator")
      }
    }
    case "*" => {
      println(s"$op for $left and $right")
      left match {
          case AtomExpression(AtomInt(_)) => right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.iimulti, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.idmulti, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }    
          case AtomExpression(AtomDouble(_)) =>  right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.dimulti, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.ddmulti, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }
          case _ => throw new RuntimeException("invalid type for " + op + " operator")
      }
    }
    case "/" => {
      println(s"$op for $left and $right")
      left match {
          case AtomExpression(AtomInt(_)) => right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.iidivide, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.iddivide, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }    
          case AtomExpression(AtomDouble(_)) =>  right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.didivide, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.dddivide, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }
          case _ => throw new RuntimeException("invalid type for " + op + " operator")
      }
    }
    case "%" => {
      println(s"$op for $left and $right")
      left match {
          case AtomExpression(AtomInt(_)) => right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.iimod, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.idmod, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }    
          case AtomExpression(AtomDouble(_)) =>  right match {
            case AtomExpression(AtomInt(_)) => BinaryOperatorExpression(Operations.dimod, left, right)
            case AtomExpression(AtomDouble(_)) => BinaryOperatorExpression(Operations.ddmod, left, right)
            case _ => BinaryOperatorExpression(Operations.ddminus, left, right)
          }
          case _ => throw new RuntimeException("invalid type for " + op + " operator")
      }
    }
    case _ => throw new RuntimeException(s"no match for operator $op")
  }
  
  /*
   * @return the printed string of parser error
   */
  //private def parserError(position: MetaData, argument: AST): Expression = {
    
  //}
}