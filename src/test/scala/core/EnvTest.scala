package test.scala.core

import main.scala.util._

object EnvTest {
  def main(args: Array[String]): Unit = {
    config.env0.extendEnvironment
    
    println(config.env0.lookUp("#f"))
  }
}