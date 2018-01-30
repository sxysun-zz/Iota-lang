package test.scala.interpreter

import main.scala.core._
import main.scala.interpreter._
import main.scala.util._

object LexerTest {
  
  def main(args: Array[String]): Unit = {
    println(config.testCode)
    beautyPrint(config.tokens)
  }
  
  def beautyPrint(l: List[Token]) = {
    println(l map ("\""+ _.value + "\""))
    println(l map ("\""+ _.property.lineNumber + "\""))
  }
}