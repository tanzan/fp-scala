/**
  * Created by serg on 2/15/17.
  */
import collection._
import scala.collection.generic.CanBuildFrom

class PrefixMap[T] extends mutable.Map[String, T] with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value:Option[T] = None

  def get(key: String): Option[T] =
    if (key.isEmpty) value
    else suffixes get (key(0)) flatMap(_.get(key substring 1))

  def withPrefix(prefix:String):PrefixMap[T] =
    if (prefix.isEmpty) this
    else {
      val leading = prefix(0)
      suffixes get(leading) match {
        case None =>
          suffixes += leading -> empty
        case _ =>
      }
      suffixes(leading) withPrefix (prefix substring 1)
    }

  override def update(key:String, value:T):Unit =
    withPrefix(key).value = Some(value)

  override def remove(key: String): Option[T] =
    if (key.isEmpty) {
      val prev = value
      value = None
      prev
    } else suffixes get(key(0)) flatMap (_.remove(key substring 1))

  override def +=(kv: (String, T)): this.type = {
    update(kv._1, kv._2)
    this
  }

  override def -=(key: String): this.type = {
    remove(key)
    this
  }

  override def iterator: Iterator[(String, T)] =
    (for(v <- value.iterator) yield ("", v)) ++
      (for((c, m) <- suffixes.iterator; (s,v) <- m.iterator) yield (c +: s, v))

  override def empty: PrefixMap[T] = new PrefixMap[T]
}

object PrefixMap {

  def empty[T] = new PrefixMap[T]

  def apply[T](entries: (String, T) *): PrefixMap[T] = {
    val map = empty[T]
    for(kv <- entries) map += kv
    map
  }

  def newBuilder[T]:mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty[T])

  implicit def canBuildFrom[T]:CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {

      def apply(from: PrefixMap[_]): mutable.Builder[(String, T), PrefixMap[T]] = newBuilder[T]

      def apply(): mutable.Builder[(String, T), PrefixMap[T]] = newBuilder[T]
    }
}
