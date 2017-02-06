
/**
  * Created by serg on 2/3/17.
  */
trait MyQueue[+T] {

  def head:T

  def tail:MyQueue[T]

  def enqueue[U >: T](x:U):MyQueue[U]

}

object MyQueue {

  private class MyQueueImp[T] (private val leading:List[T],
                               private val trailing:List[T]) extends MyQueue[T] {

    private def mirror:MyQueueImp[T] =
      if (leading.isEmpty)
        new MyQueueImp(trailing.reverse, Nil)
      else
        this


    def head:T = mirror.leading.head

    def tail:MyQueueImp[T] = new MyQueueImp(mirror.leading.tail, trailing)

    def enqueue[U >: T](x:U):MyQueue[U] = new MyQueueImp(mirror.leading, x::mirror.trailing)
  }

  def apply[T](elems:T*):MyQueue[T] = new MyQueueImp(elems.toList, Nil)
}

trait OutputChannel[-T] {
  def write(x:T):Unit
}

class StringChannel extends OutputChannel[String] {
  override def write(x: String): Unit = println(x)
}

class AnyRefChannel extends OutputChannel[AnyRef] {
  override def write(x: AnyRef): Unit = println(x)
}



object MyQueueMain {

  def main(args: Array[String]): Unit = {
    val q = MyQueue[Int]()
    val q1 = q.enqueue(1).enqueue(2).enqueue(3)

    println(q1.head)
    println(q1.tail.head)


    class Plant

    class Fruit extends Plant

    class Apple extends Fruit

    class Orange extends Fruit

    val apples = MyQueue(new Apple)

    val fruits:MyQueue[Fruit] = apples.enqueue(new Orange)

    val anyRefChannel:AnyRefChannel = new AnyRefChannel

    val stringChannel:StringChannel = new StringChannel

    val c1:OutputChannel[String] = anyRefChannel



  }
}
