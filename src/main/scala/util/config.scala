package main.scala.util

object config {
  
  import main.scala.interpreter._
  import main.scala.core._
  
  val testFileName = "sample.iota"
  val testPath = "./"+testFileName
  
  val beforeMacro = diskOperator.load(testPath)
  
  val testCode = Macro(beforeMacro).initializeRaw()
  
  /**
   * environment default
   */
  val env0 = Environment()
  
  /**
   * lexical analyzer
   */
  val tokens = Lexer(config.testCode).getTokens()
  
  /**
   * parser abstract syntax tree
   */
  val parser = Parser(tokens, env0)
}