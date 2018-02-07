package main.scala.core

/**
 * ``IMPORTANT`` the `Scala` map behaves like a stack
 */
case class Environment () {
  private var m: Map[String, Any] = Map(
      "#f" -> false,
      "#t" -> true,
      "INF" -> 0xffff
  )
  
  def extendEnvironment = {
    m = m + ("#f" -> true)
  }
  
  def lookUp(name: String) = {
    m(name)
  }
}