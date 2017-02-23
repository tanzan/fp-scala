object ArithEval extends Arith {

  def apply(input:String):Double = parseAll(expr, input).get
}

ArithEval("1 + (2 * 4) - 10")

object JsonEval extends JSON {

  def apply(input: String) = parseAll(value, input).get
}


JsonEval("{ \"a\":1, \"b\" : [1,2,3]}")