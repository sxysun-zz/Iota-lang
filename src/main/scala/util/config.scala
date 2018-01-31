package main.scala.util

object config {
  
  import main.scala.interpreter._
  import main.scala.core._
  
  val testFileName = "a.aly"
  val testPath = "./"+testFileName
  /**
   * indicates the end of an alayi-lang file
   */
  val codeEnd = "\n ■"
  /**
   * the rawCode string
   */
  val testCode = seperation(diskOperator.load(testPath))++codeEnd
  
  def seperation(s: String) = s.replace("(", " ( ").replace(")", " ) ")
  
  /**
   * lexical analyzer
   */
  val tokens = Lexer(config.testCode).getTokens()
  /**
   * parser abstract syntax tree
   */
  val ast = Parser(tokens).getAST()
  /**
   * evaluator
   */
  //val eval = Evaluator(ast)
  /**
   * environment default
   */
  val env0 = Environment()
}