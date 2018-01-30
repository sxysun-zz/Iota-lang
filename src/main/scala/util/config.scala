package main.scala.util

object config {
  val testFileName = "a.aly"
  val testPath = "./"+testFileName
  val testCode = diskOperator.load(testPath).replace("(", "( ").replace(")", " )")
}