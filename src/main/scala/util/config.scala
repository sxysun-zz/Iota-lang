package main.scala.util

object config {
  
  import main.scala.interpreter._
  import main.scala.core._
  
  val testFileName = "a.aly"
  val testPath = "./"+testFileName
  
  val beforeMacro = diskOperator.load(testPath)
  
  val testCode = Macro(beforeMacro).initializeRaw()
  
  /**
   * lexical analyzer
   */
  val tokens = Lexer(config.testCode).getTokens()
  /**
   * parser abstract syntax tree
   */
  val expr = Parser(tokens).getSExpression()
  /**
   * evaluator
   */
  val eval = Evaluator(expr).mainEval(Environment())
  /**
   * environment default
   */
  val env0 = Environment()
  
  val typeT = Parser(tokens).typeInferenceTest()
}