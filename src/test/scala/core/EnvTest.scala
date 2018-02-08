package test.scala.core

import main.scala.util._

object EnvTest {
  def main(args: Array[String]): Unit = {
    println(config.env0.lookUp("#f"))
  }
}