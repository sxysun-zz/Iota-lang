package main.scala.interpreter

case class Macro (raw: String){
  
  private val codeEnd = "\n ■"
  
  private val defaultMacro: List[Tuple2[String, String]] = List(
    ("(", " ( "),
    (")", " ) "),
    ("\\ge", "≥"),
    ("\\le", "≤"),
    ("\\equiv", "≡"),
    ("\\eq", "等"),
    ("\\neg", "¬"),
    ("\\ne", "≠"),
    ("\\or", "∨"),
    ("\\and", "∧"),
    ("\\lambda", "λ"),
    ("\\to", "→"),
    ("#t", "真"),
    ("#f", "假")
  )
  
  def initializeRaw(): String = {
    var get = raw
    defaultMacro.map(x => get = get.replace(x._1, x._2))
    get++codeEnd
  }
}