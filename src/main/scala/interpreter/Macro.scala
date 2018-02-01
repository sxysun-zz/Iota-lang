package main.scala.interpreter

case class Macro (raw: String){
  
  private val codeEnd = "\n ■"
  
  private val defaultMacro: List[Tuple2[String, String]] = List(
    ("(", " ( "),
    (")", " ) "),
    ("\\ge", "≥"),
    ("\\le", "≤"),
    ("\\equiv", "≡"),
    ("\\neg", "¬"),
    ("\\ne", "≠"),
    ("\\iff", "↔"),
    ("\\vdash", "⊢"),
    ("\\or", "∨"),
    ("\\and", "∧"),
    ("\\lambda", "λ"),
    ("\\to", "→")
  )
  
  def initializeRaw(): String = {
    var get = raw
    defaultMacro.map(x => get = get.replace(x._1, x._2))
    get++codeEnd
  }
}