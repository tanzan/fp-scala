///// Singleton type
object Foo

val f:Foo.type = Foo

class A

val a = new A

val b:a.type = a

// val c:a.type = new A doesn't compile


import shapeless.syntax.singleton._

var x = 41.narrow

// x = 42 doesn't compile





