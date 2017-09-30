import scala.reflect.ClassTag

def foo[A, B](a:A => Int, b:B):Int = 1

def foo[A, B](b:B)(a:A => Int):Int = 1


//def bar(a:Int, b:Int)  = 1

//def bar(a:Int)(b:Int) = 1

//val x = bar _

//(bar _)(1)

foo((x:Int) => 1, 1)
