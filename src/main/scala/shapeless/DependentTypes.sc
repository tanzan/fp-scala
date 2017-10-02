import shapeless.{::, HList, HNil, the}
import shapeless.ops.hlist.Last
val last1 = Last[String :: Int :: HNil]
val last2 = Last[Int :: String :: HNil]

last1("foo" :: 123 :: HNil)
last2(321 :: "bar" :: HNil)

// last1(321 :: "bar" :: HNil) doesn't compile

trait Second[L <: HList] {
  type Out
  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] { type Out = O } // saves Out from erasure

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] =
    inst
}

implicitly[Last[String :: Int :: HNil]] // this erases Out type !

Last[String :: Int :: HNil] // but this not

the[Last[String :: Int :: HNil]]

implicit def hlistSecond[A, B, Rest <: HList]: Second.Aux[A :: B :: Rest, B] =
  new Second[A :: B :: Rest] {
    type Out = B
    def apply(value: A :: B :: Rest): B =
      value.tail.head
  }

val second1 = Second[String :: Boolean :: Int :: HNil]
val second2 = Second[String :: Int :: Boolean :: HNil]

second1("foo" :: true :: 123 :: HNil)
second2("bar" :: 321 :: false :: HNil)

// second1("baz" :: HNil) this doesn't compile