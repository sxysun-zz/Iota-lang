package main.scala.interpreter

import scala.annotation.Annotation
import main.scala.core._
import scala.collection.mutable._

case class Parser (tokens: List[Token], env: Environment) {
  
  private var root: FragileRoot = FragileRoot(ListBuffer(FragileNode(tokens.head, ListBuffer(), null)))
  private var currentNode = root.children.head
  private var parseTimeSymbolTable = TypeEnvironment().copyExternal(env)
  
  /**
   * `WARNING` not guaranteed to be a right syntax tree
   * `IMPORTANT` this function has side effect
   * @return a crude crafted abstract syntax tree
   */
  def getRawAST(): FragileRoot = {
    import dfaState._
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
        if(currentNode.parent == null) {
          l.drop(1).head match {
            case t@Token(END, _,_) => {
              //println("get raw abstract syntax tree completed")
              Unit
            }
            case _ => {
              root.children.append(FragileNode(l.drop(1).head, ListBuffer(), null))
              currentNode = root.children.last
              getTail(l.drop(2))
            }
          }
        } else {
          currentNode = currentNode.parent
          getTail(l.drop(1))
        }
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
  def getSExpression(): List[Expression] = {
    root = getRawAST()
    //build up the overall symbol table for parser type check
    root.checkType(parseTimeSymbolTable)
    root.children.map(get(_, parseTimeSymbolTable)).toList
  }
  
  import dfaState._
  /**
   * @return a recursive decent parser
   */
  private def get(ast: AST, env: TypeEnvironment): Expression = ast match {
      case FragileRoot(c) => get(c.head, env)
      case FragileNode(v, cs, p) => cs.head match{
        
        //take care of all procedures and lambda expressions
        
        //lambda expression
        case AtomicNode(Token(LAMBDA, content, prop), _) => {
          var varName: String = ""
          cs.drop(1).head match {
            case FragileNode(v_, cs_, p_) => cs_.head match {
              case AtomicNode(Token(IDENTIFIER, _content, _prop), _) => varName = _content
              case _ => throw new RuntimeException("wrong lambda expression identifier type" + 
                 s" at $p, current argument is $cs")
            }
            case _ => throw new RuntimeException(s"wrong lambda expression at $prop")
          }
          val newEnv = establishVarNameType(varName, env, cs.drop(2).head)
          LambdaExpression(varName, get(cs.drop(2).head, newEnv))
        }
        
        //define expression
        case AtomicNode(Token(DEFINE, content, prop), _) => {
          var varName: String = ""
          cs.drop(1).head match {
            case AtomicNode(Token(IDENTIFIER, _content, _prop), _) => varName = _content
            case _ => throw new RuntimeException("wrong define clause variable name" + 
                s" at $p, current argument is $cs")
          }
          val attr = get(cs.drop(2).head, env)
          DefineExpression(varName, attr)
        }
        
        //arithmetic binary operator expression
        case AtomicNode(Token(ARITHOPERATOR, content, prop), _) => {
           val opType = ast.checkType(env)
           val left = get(cs.drop(1).head, env)
           val right = get(cs.drop(2).head, env)
           parseArithmeticOp(content, left, right, opType)
        }
        
        //boolean binary operator expression
        case AtomicNode(Token(BOOLOPERATOR, content, prop), _) => {
          val left = get(cs.drop(1).head, env)
          val right = get(cs.drop(2).head, env)
          parseBooleanOp(content, left, right)
        }
        
        //an unclosed operation
        case AtomicNode(Token(COMPOPERATOR, content, prop), _) => {
          val left = get(cs.drop(1).head, env)
          val right = get(cs.drop(2).head, env)
          parseComparatorOp(content, left, right)
        }
        
        //closure: defined function application expression
        case AtomicNode(Token(IDENTIFIER, content, prop), _) => {
          if(cs.length != 3) throw new RuntimeException("wrong number of arguments in defined closure function " + 
              s"application, location in $p, current argument is $cs")
          val funcName = get(cs.head, env)
          val body = get(cs.drop(1).head, env)
          ApplicationExpression(funcName, body)
        }
        
        //real-time function application expression
        case FragileNode(value, content, prop) => {
          val func = get(cs.head, env)
          val body = get(cs.drop(1).head, env)
          import langType._
          if(cs.head.checkType(env) != lambda) throw new RuntimeException("non-lambda typed expression" + 
              "appeared in function application position, location in $p, current argument is $cs")
          ApplicationExpression(func, body)
        }
        
        //if expression
        case AtomicNode(Token(IF, content, prop), _) => {
          val premises = get(cs.drop(1).head, env)
          val trueJump = get(cs.drop(2).head, env)
          val falseJump = get(cs.drop(3).head, env)
          IfExpression(premises, trueJump, falseJump)
        }
        
        //error expression
        case _ => {
          throw new RuntimeException("ill formed expression in parser")
        }
      }
      //atomic expression
      case AtomicNode(v, p) => v match {
        case Token(IDENTIFIER, content, prop) => {AtomExpression(AtomIdentifier(content))}
        case Token(INT, content, prop) => {AtomExpression(AtomInt(content.toInt))}
        case Token(DOUBLE, content, prop) => {AtomExpression(AtomDouble(content.toDouble))}
        case Token(STRING, content, prop) => {AtomExpression(AtomString(content))}
        case Token(BOOLEAN, content, prop) => {AtomExpression(AtomBoolean(content.toBoolean))}
        case Token(k, content, prop) => {throw new RuntimeException("wrong atomic node" +
	        s"in syntax tree at $prop, token is $content, token type is $k")
        }
      }
    }
  
  /**
   * @return the lambda support for operator and atom only now..
   */
  private def establishVarNameType(
      varName: String, env: TypeEnvironment, body: AST): TypeEnvironment = body match {
    case FragileNode(v, cs, p) => cs.head match {
      case AtomicNode(Token(dfaState.ARITHOPERATOR, content, prop), _) => 
        TypeEnvironment().copyExternal(env).extendEnvironment(varName, langType.double)
      case AtomicNode(Token(dfaState.BOOLOPERATOR, content, prop), _) => 
        TypeEnvironment().copyExternal(env).extendEnvironment(varName, langType.boolean)
      case lam@AtomicNode(Token(dfaState.LAMBDA, content, prop), _) => cs.drop(1).head match {
// support for currying ----------------------------------------------
        case FragileNode(_, ListBuffer(AtomicNode(Token(dfaState.IDENTIFIER, c_, p_), _), _*), _) => 
          establishVarNameType(c_, env, cs.drop(2).head)
        case _ => throw new RuntimeException("no value was found")
      }
      case unknown@FragileNode(v_, cs_, p_) => establishVarNameType(varName, env, unknown)
    	case AtomicNode(Token(dfaState.IF, content, prop), _) => 
    	  TypeEnvironment().copyExternal(env).extendEnvironment(varName, cs.drop(2).head.checkType(env))
      case _ => throw new RuntimeException(s"ill-formed expression in lambda $body")
    }
    case AtomicNode(v, p) => v match {
        case Token(dfaState.IDENTIFIER, content, prop) => env.lookUp(content) match {
          case Right(v_) => TypeEnvironment().copyExternal(env).extendEnvironment(varName, v_)
          case _ => throw new RuntimeException(s"$content not found in lambda expression at $p")
        }
        case Token(s@dfaState, content, prop) => 
          TypeEnvironment().copyExternal(env).extendEnvironment(varName, typeReflex.correspondingType(s))
      }
    case _ => throw new RuntimeException(s"ill-formed expression in lambda $body")
  }
  
  private def parseBooleanOp(op: String, left: Expression, right: Expression): Expression = op match {
    case "≡" => BinaryOperatorExpression(Operations.equiv, left, right)
    case "∨" => BinaryOperatorExpression(Operations.or, left, right)
    case "∧" => BinaryOperatorExpression(Operations.and, left, right)
    case "≠" => BinaryOperatorExpression(Operations.xor, left, right)
    case _ => throw new RuntimeException(s"no match for operator $op")
  }
  
  import langType._
  private def parseComparatorOp(op: String, left: Expression, right: Expression): Expression = op match {
    case "<" => UnclosedOperationExpression(Operations.ddSm, left, right)
    case ">" => UnclosedOperationExpression(Operations.ddGe, left, right)
    case "≠" => UnclosedOperationExpression(Operations.ddnumInEquiv, left, right)
    case "==" => UnclosedOperationExpression(Operations.ddnumEquiv, left, right)
    case _ => throw new RuntimeException(s"no match for operator $op")
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