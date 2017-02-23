import scala.util.parsing.combinator._

/**
  * Created by serg on 2/21/17.
  */

class Arith extends JavaTokenParsers {

  def expr:Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case head ~ tail => tail.foldLeft(head)((right, left) => left match {
      case "+" ~ operand => right + operand
      case "-" ~ operand => right - operand
    })
  }
  def term:Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case head ~ tail => tail.foldLeft(head)((right, left) => left match {
      case "*" ~ operand => right * operand
      case "/" ~ operand => right / operand
    })
  }
  def factor:Parser[Double] = floatingPointNumber ^^ (_.toDouble) | "(" ~> expr <~ ")" ^^ (x => x)
}


class JSON extends JavaTokenParsers {
  def value:Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "false" | "true"
  def obj:Parser[Any] = "{" ~> members <~ "}"
  def arr:Parser[Any] = "[" ~> values <~ "]"
  def members:Parser[Map[String,Any]] = repsep(member, ",") ^^ (Map() ++ _)
  def member:Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }
  def values:Parser[List[Any]] = repsep(value, ",")
}