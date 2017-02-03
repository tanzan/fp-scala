import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by serg on 2/3/17.
  */
object Traits {

  class A(val x:Int)

  trait B extends A

  class C extends A(1) with B {
    println(x)
  }

  trait IntQueue {
    def get(): Int
    def put(x: Int)
  }

  class BasicIntQueue extends IntQueue {
    private val buff = ListBuffer[Int]()
    def get(): Int = buff.remove(0)
    def put(x: Int): Unit = buff += x
  }

  trait DoublingQueue extends IntQueue {
    abstract override def put(x:Int) = super.put(x * 2)
  }

  trait Incrementing extends IntQueue {
    abstract override def put(x:Int) = super.put(x + 1)
  }

  class Animal {
    def who:String = "Animal"
  }
  trait Furry extends Animal {
    override def who:String = "Furry"
  }
  trait HasLegs extends Animal {
    override def who:String = "Has Legs"
  }
  trait FourLegged extends HasLegs {
    override def who: String = "Four Legged"
  }

  class Cat extends Animal with FourLegged with Furry


  def main(args: Array[String]): Unit = {
    val q = new BasicIntQueue with Incrementing with DoublingQueue
    q.put(1)
    q.put(2)
    println(q.get())
    println(q.get())

    println((new Cat).who)
  }
}
