package main.scala.util

object config {
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
}