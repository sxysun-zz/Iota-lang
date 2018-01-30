package main.scala.interpreter

import main.scala.core._

case class Lexer (rawCode: String){
  private var currentLine = 0
  private var currentCol = 0
  private var code = rawCode
  private var currentPos = 0
  
  private def getProp = MetaData(currentLine, currentCol)
  private def nowChar = code.charAt(currentPos)
  private def moveChar = {currentPos = currentPos + 1}
  
  /**
   * returns a list of tokens, uses deterministic finite automata method
   */
  def getTokens(): List[Token] = {
    import dfaState._
    def getTail(state: dfaState, l: List[Token]): List[Token] = state match {
      case START => nowChar match {
        case '(' => {moveChar;getTail(START, l:+Token(LPAREN, "(", getProp))}
        case ')' => {moveChar;getTail(START, l:+Token(RPAREN, ")", getProp))}
        case '/' => {moveChar;getTail(LCOMMENT, l)}
        case '*' => {moveChar;getTail(START, l:+Token(BINOPERATOR, "*", getProp))}
        case '%' => {moveChar;getTail(START, l:+Token(BINOPERATOR, "%", getProp))}
        case '+' => {moveChar;getTail(START, l:+Token(BINOPERATOR, "+", getProp))}
        case '-' => {moveChar;getTail(START, l:+Token(BINOPERATOR, "-", getProp))}
        case '=' => {moveChar;getTail(START, l:+Token(BINOPERATOR, "=", getProp))}
        case '>' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "=", getProp))}
        case '<' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "=", getProp))}
        case '≥' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "≥", getProp))}
        case '≤' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "≤", getProp))}
        case '≡' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "≡", getProp))}
        case '¬' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "¬", getProp))}
        case '≠' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "¬", getProp))}
        case '↔' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "↔", getProp))}
        case '⊢' => {moveChar;getTail(START, l:+Token(BOOLOPERATOR, "⊢", getProp))}
        case _ => {
          //space
          if(nowChar.isSpaceChar) {moveChar;getTail(START, l)}
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
          else {moveChar;getTail(END, l)}
        }
      }
      
      case LCOMMENT => nowChar match {
        case '/' => {cleanLine;getTail(START, l)}
        case '*' => {cleanBlock;getTail(START, l)}
        case _ => {
          if(nowChar.isSpaceChar) {moveChar;getTail(START, l:+Token(BINOPERATOR, "/", getProp))}
          else {moveChar;getTail(END, l)}
        }
      }
      
      case _ => l
    }
    getTail(START, List())
  }
  
  private def getIdentifier = {
    def getTail(n: String): String = {
      moveChar
      if(!nowChar.isSpaceChar) {n++nowChar.toString()}
      else n
    }
    getTail("")
  }
  
  private def getNumber = {
    var isDouble = false
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
   * side effect of changing the `currentPos` variable
   */
  private def cleanLine = {
    def cleanTail(): Unit = if(!inList(nowChar, endLineChar)) {moveChar;cleanTail()}
    cleanTail()
  }
  
  private def cleanBlock = {
    currentPos = code.substring(currentPos).indexOf("*/") + 2
  }
  
  private def inList [A] (v: A, l: List[A]): Boolean = (false /: (l map (_==v))) (_||_)
  private val endLineChar = List('\r', '\n')
}