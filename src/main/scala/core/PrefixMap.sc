import core._

val m = PrefixMap("a" -> 10, "b" -> 50)

m += "abc" -> 0
m += "abd" -> 1
m += "al" -> 2
m += "all" -> 3
m += "xy" -> 4

m withPrefix "a"

m get("abc")

m get("a")

m("abc")

m.iterator foreach(println)

m -= "xy"
m -= "a"

m.map(kv => kv)





