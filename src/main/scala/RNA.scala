import scala.collection.{IndexedSeqLike, mutable}

/**
  * Created by serg on 2/15/17.
  */
abstract class Base
case object A extends Base
case object T extends Base
case object G extends Base
case object U extends Base

object Base {
  val fromInt: Int => Base = Array(A, T, G, U)
  val toInt: Base => Int = Map(A -> 0, T -> 1, G -> 2, U -> 3)
}

final class RNA private (groups:Array[Int], val length: Int)
  extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA] {

  import RNA._

  override protected[this] def newBuilder: mutable.Builder[Base, RNA] =
    mutable.ArrayBuffer[Base]() mapResult(buf  => fromSeq(buf))

  def apply(idx: Int): Base = {
    if (idx < 0 || idx >= length)
      throw new IndexOutOfBoundsException
    Base.fromInt(groups(idx/N) >> (idx % N * S) & M)
  }
}

object RNA {

  private val S = 2

  private val N = 32 / S

  private val M = (1 << 2) - 1

  def fromSeq(buf:Seq[Base]):RNA = {
    val groups = new Array[Int]((buf.length + N - 1)/N)
    for(i <- buf.indices) {
      groups(i / N) |= Base.toInt(buf(i)) << i % N * S
    }
    new RNA(groups, buf.length)
  }



  def apply(buf:Base *):RNA = fromSeq(buf)

}
