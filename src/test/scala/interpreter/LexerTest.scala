package test.scala.interpreter

import main.scala.interpreter._
import main.scala.util._

object LexerTest {
  def main(args: Array[String]): Unit = {
    println(config.testCode)
    println(Lexer(config.testCode).getTokens().map(_.value))
  }
}