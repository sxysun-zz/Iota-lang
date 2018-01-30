package main.scala.util

import scala.io.Source
import java.io._
import java.nio.CharBuffer

object diskOperator {
  
  def load(path: String): String = Source.fromFile(path).mkString
  
  def writeToLocalDisk(path: String, content: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(path)))
    writer.write(content)
    writer.close()
  }
  
  def loadTextFromDisk(path: String): String = {
    val reader = new BufferedReader(new FileReader(new File(path)))
    val ret : StringBuilder = new StringBuilder()
    def readTail(): String = {
      val line = reader.readLine()
      if(line == ""){
        ret.toString()
      } else {
        ret append reader.readLine()
        readTail()
      }
    }
    readTail()
  }
  
}