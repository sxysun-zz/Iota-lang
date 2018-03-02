package main.scala.interpreter

import scala.annotation.Annotation
import main.scala.core._

case class Lexer (rawCode: String){
  private var currentLine = 0
  private var code = rawCode
  private var currentPos = 0
  
  /**
   * `IMPORTANT` need better friendlier implementation and error message
   * @return the information about current code, used for debugging
   */
  private def getProp = MetaData(currentLine/2, currentPos)
  private def nowChar = code.charAt(currentPos)
  private def moveChar = {currentPos = currentPos + 1}
  
  /**
   * `IMPORTANT`: this function has side effect
   * @return the identifier string if applicable
   */
  private def getIdentifier: String = {
    @annotation.tailrec
    def getTail(n: String): String = {
      moveChar
      if(!nowChar.isSpaceChar && !inList(nowChar, endLineChar)) 
        {getTail(n++nowChar.toString())}
      else n
    }
    getTail("")
  }
  
  /**
   * `IMPORTANT`: this function has side effect
   * `WARNING`: this function does not check if the number is valid or not
   * @return Tuple2[Boolean, String], the first is false if is integer, the second is the string representation
   */
  private def getNumber = {
    var isDouble = false
    @annotation.tailrec
    def getTail(n: String): String = {
      moveChar
      if(nowChar.isDigit) {getTail(n++nowChar.toString())} 
      else if (nowChar == '.') {isDouble=true;getTail(n++nowChar.toString())}
      else n
    }
    val num = getTail("")
    (isDouble, num)
  }
  
  /**
   * `IMPORTANT`: side effect of changing the `currentPos` and `currentLine` variable
   */
  private def cleanLine = {
    @annotation.tailrec
    def cleanTail(): Unit = if(!inList(nowChar, endLineChar)) {moveChar;cleanTail()}
    cleanTail()
    currentLine = currentLine + 1
  }
  
  /**
   * `IMPORTANT`: side effect of changing the `currentPos` and `currentLine` variable
   */
  private def cleanBlock = {
    moveChar
    @annotation.tailrec
    def get(): Unit = {
      if(nowChar == '*' && code.charAt(currentPos + 1) == '/') {
        moveChar; moveChar
      } else {
        if(inList(nowChar, endLineChar)) currentLine = currentLine + 1
        moveChar; get()
      }
    }
    get()
  }
  
  /**
   * used to obtain a string literal
   * @return the stripped string from source code file
   */
  private def getStringLiteral: String = {
    def get(s: String, p: Unit): String = {
      if(nowChar == '"') {moveChar; s}
      else get(s + nowChar, moveChar)
    }
    get("", moveChar)
  }
  
  /**
   * some higher order function, written purely for artistic reason :)
   * @return check if an element is in a list
   */
  private def inList [A] (v: A, l: List[A]): Boolean = (false /: (l map (_==v))) (_||_)
  private val endLineChar = List('\r', '\n')
  
  /**
   * `WARNING` the tokens are not guaranteed to be valid, such as the parentheses number may be wrong
   * @return a list of tokens, uses deterministic finite automata method
   */
  def getTokens(): List[Token] = {
    import dfaState._
    @annotation.tailrec
    def getTail(state: dfaState, l: List[Token]): List[Token] = state match {
      case START => nowChar match {
        
        case '(' => {moveChar;getTail(START, l:+Token(LPAREN, "(", getProp))}
        case ')' => {moveChar;getTail(START, l:+Token(RPAREN, ")", getProp))}
        
        case '/' => {moveChar;getTail(LCOMMENT, l)}
        
        case '=' => {moveChar;getTail(START, l:+Token(DEFINE, "=", getProp))}
        
        case '*' => {moveChar;getTail(START, l:+Token(ARITHOPERATOR, "*", getProp))}
        case '%' => {moveChar;getTail(START, l:+Token(ARITHOPERATOR, "%", getProp))}
        case '+' => {moveChar;getTail(START, l:+Token(ARITHOPERATOR, "+", getProp))}
        case '-' => {moveChar;getTail(START, l:+Token(ARITHOPERATOR, "-", getProp))}
        
        case '>' => {moveChar;getTail(START, l:+Token(COMPOPERATOR, ">", getProp))}
        case '<' => {moveChar;getTail(START, l:+Token(COMPOPERATOR, "<", getProp))}
        case '≥' => {moveChar;getTail(START, l:+Token(COMPOPERATOR, "≥", getProp))}
        case '≤' => {moveChar;getTail(START, l:+Token(COMPOPERATOR, "≤", getProp))}
        case '≠' => {moveChar;getTail(START, l:+Token(COMPOPERATOR, "≠", getProp))}
        case '等' => {moveChar;getTail(START, l:+Token(COMPOPERATOR, "==", getProp))}
        
        case '≡' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "≡", getProp))}
        case '¬' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "¬", getProp))}
        case '∨' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "∨", getProp))}
        case '∧' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "∧", getProp))}
        
        case 'λ' => {moveChar;getTail(START, l:+Token(LAMBDA, "λ", getProp))}
        
        case '"' => {getTail(START, l:+Token(STRING, getStringLiteral, getProp))}
        
        case '真' => {moveChar;getTail(START, l:+Token(BOOLEAN, "true", getProp))}
        case '假' => {moveChar;getTail(START, l:+Token(BOOLEAN, "false", getProp))}
        
        case '■' => {getTail(END, l:+Token(END, "■", getProp))}
        case _ => {
          //DEBUG
          //println(s"$nowChar")
          //space
          if(nowChar.isSpaceChar) {
            //if(inList(nowChar, endLineChar)) currentLine = currentLine + 1
            moveChar;getTail(START, l)
          }
          else if(nowChar == 'i') {
            if(code.charAt(currentPos + 1) == 'f' && code.charAt(currentPos + 2).isSpaceChar)
             {moveChar;moveChar;getTail(START, l:+Token(IF, "if", getProp))}
            else {currentPos = currentPos - 1
              getTail(START, l:+Token(IDENTIFIER, getIdentifier, getProp))}
          }
          //integer or double
          else if (nowChar.isDigit) {
            currentPos = currentPos - 1
            val t = getNumber
            if(t._1) getTail(START, l:+Token(DOUBLE, t._2, getProp))
            else getTail(START, l:+Token(INT, t._2, getProp))}
          //identifier
          else if (nowChar.isLetter) {
            currentPos = currentPos - 1
            getTail(START, l:+Token(IDENTIFIER, getIdentifier, getProp))
          }
          //other charaters
          else {
            if(inList(nowChar, endLineChar)) currentLine = currentLine + 1
            moveChar;getTail(START, l)
          }
        }
      }
      
      case LCOMMENT => nowChar match {
        case '/' => {cleanLine;getTail(START, l)}
        case '*' => {cleanBlock;getTail(START, l)}
        case _ => {
          if(nowChar.isSpaceChar) {moveChar;getTail(START, l:+Token(ARITHOPERATOR, "/", getProp))}
          else {moveChar;getTail(END, l)}
        }
      }
      
      case _ => l
    }
    getTail(START, List())
  }
}