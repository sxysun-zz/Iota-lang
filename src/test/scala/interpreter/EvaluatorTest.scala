package test.scala.interpreter

import main.scala.interpreter._
import main.scala.core._
import main.scala.util._

object EvaluatorTest {
  def main(args: Array[String]): Unit = {
    val evaled = Evaluator(config.expr).mainEval(config.env0)
    printEvaluationResult(evaled)
  }
  
  def printEvaluationResult(l: List[Any]): Unit = {
    l.zipWithIndex.map(x => println("iota-lang> " + (x._2+1) + "th expression: " + x._1))
  }
}