package test.scala.interpreter

import main.scala.util._

object ParserTest {
  import LexerTest._
  def main(args: Array[String]): Unit = {
    println(config.parser.getSExpression.size)
    //println(config.expr.drop(3).head.inferType(config.env0))
  }
}