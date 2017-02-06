/**
  * Created by serg on 2/2/17.
  */
trait MyList[+T] {

  def head:T

  def tail: MyList[T]

  def isEmpty():Boolean

  def ::[B >: T](h:B):MyList[B] = new ::(h, this)

  /*def :::[B >: T](list:MyList[B]):MyList[B] = {
    def iterate(l:MyList[T], r:MyList[B]):MyList[B] = {

    }
    iterate()
  }*/

  def reverse:MyList[T] = {
    def iterate(l:MyList[T], r:MyList[T]):MyList[T] = {
      if (l.isEmpty()) r
      else iterate(l.tail, l.head :: r)
    }
    iterate(this, MyNil)
  }

  override def toString: String = {
    def iterate(l:MyList[_], s: String):String = {
      if (l.isEmpty()) s + "Nil"
      else iterate(l.tail, s  + l.head.toString + "::")
    }
    iterate(this, "")
  }

}

object MyNil extends MyList[Nothing] {
  def head: Nothing =  throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new UnsupportedOperationException

  def isEmpty(): Boolean = true
}

case class ::[T](h:T, t:MyList[T]) extends MyList[T] {

  def head: T = h

  def tail: MyList[T] = t

  def isEmpty(): Boolean = false
}

class A

class B extends A

object Main {
  def main(args: Array[String]): Unit = {
    println(1::2::3::MyNil)
    println((1::2::3::MyNil).reverse)

  }
}
