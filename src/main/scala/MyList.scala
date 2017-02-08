import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by serg on 2/2/17.
  */
abstract class MyList[+T] {

  def head:T

  def tail: MyList[T]

  def isEmpty:Boolean

  def ::[B >: T](h:B):MyList[B] = new ::(h, this)

  def :::[B >: T](prefix:MyList[B]):MyList[B] =
    if (prefix.isEmpty) this
    else prefix.head::prefix.tail:::this

  def size:Int =
    if(isEmpty) 0 else 1 + tail.size


  def drop(n:Int):MyList[T] =
    if (n <= 0) this
    else tail.drop(n - 1)

  def dropRight(n:Int):MyList[T] = {
    def copy(l:MyList[T], r:MyList[T]):MyList[T] =
      if (r.isEmpty) MyNil
      else l.head::copy(l.tail, r.tail)
    copy(this, drop(n))
  }

  def take(n:Int):MyList[T] = {
    if(n <= 0) MyNil
    else head::tail.take(n - 1)
  }

  def map[B](f:T => B):MyList[B] =
    if (isEmpty) MyNil
    else f(head) :: tail.map(f)

  def flatMap[B](f: T => MyList[B]):MyList[B] =
    if (isEmpty) MyNil
    else f(head) ::: tail.flatMap(f)

  def reverse:MyList[T] = {
    @tailrec
    def iterate(l:MyList[T], r:MyList[T]):MyList[T] = {
      if (l.isEmpty) r
      else iterate(l.tail, l.head :: r)
    }
    iterate(this, MyNil)
  }

  override def toString: String = {
    def iterate(l:MyList[_], s: String):String = {
      if (l.isEmpty) s + ")"
      else iterate(l.tail, s + l.head.toString + (if (l.tail.isEmpty) "" else  "," ))
    }
    iterate(this, "MyList(")
  }

}

case object MyNil extends MyList[Nothing] {
  def head: Nothing =  throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty(): Boolean = true
}

final case class ::[T](h:T, t:MyList[T]) extends MyList[T] {

  def head: T = h

  def tail: MyList[T] = t

  def isEmpty(): Boolean = false
}

object MyList {
  def apply[T](elems:T *):MyList[T] =
    if (elems.isEmpty) MyNil
    else elems.head :: apply[T](elems.tail:_ *)
}

object Main {
  def main(args: Array[String]): Unit = {
    println(1::2::3::MyNil)
    println((1::2::3::MyNil).reverse)

    println((1::2::MyNil):::(3::4::MyNil))
    println(MyList(1,2,3))
    println(MyList(1,2,3).drop(2))
    println(MyList(1,2,3).map(_ * 2))
    println(MyList(1,2,3).flatMap(x => MyList(x * x)))
    println(MyList(1,2,3,4).dropRight(2))
    println(MyList(1,2,3,4).take(2))

  }
}
