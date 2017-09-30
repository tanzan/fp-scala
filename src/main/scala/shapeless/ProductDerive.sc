///// type class
trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc
}

def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
  new CsvEncoder[A] {
    def encode(value: A): List[String] = func(value)
  }


def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
  values.map(encoder.encode).map(_.mkString(",")).mkString("\n")

///// HList encoder

implicit val stringEncoder: CsvEncoder[String] =
  createEncoder(str => List(str))

implicit val intEncoder: CsvEncoder[Int] =
  createEncoder(num => List(num.toString))

implicit val booleanEncoder: CsvEncoder[Boolean] =
  createEncoder(bool => List(if(bool) "yes" else "no"))

import shapeless.{HList, ::, HNil}

implicit val hnilEncoder: CsvEncoder[HNil] =
  createEncoder(hnil => Nil)

implicit def hlistEncoder[H, T <: HList](
                                          implicit
                                          hEncoder: CsvEncoder[H],
                                          tEncoder: CsvEncoder[T]
                                        ): CsvEncoder[H :: T] =
  createEncoder {
    case h :: t =>
      hEncoder.encode(h) ++ tEncoder.encode(t)
  }


val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] =
  implicitly

reprEncoder.encode("abc" :: 123 :: true :: HNil)

///// product encoder

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val iceCreams: List[IceCream] = List(
  IceCream("Sundae", 1, false),
  IceCream("Cornetto", 0, true),
  IceCream("Banana Split", 0, false)
)

case class Employee(name: String, number: Int, manager: Boolean)

val employees: List[Employee] = List(
  Employee("Bill", 1, true),
  Employee("Peter", 2, false),
  Employee("Milton", 3, false)
)

import shapeless.Generic

implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  val gen = Generic[IceCream]
  val enc = CsvEncoder[gen.Repr]
  createEncoder(iceCream => enc.encode(gen.to(iceCream)))
}

writeCsv(iceCreams)

///// generic product encoder

//implicit def genericEncoder[A, R](
//                                implicit
//                                gen: Generic[A] { type Repr = R },
//                                enc: CsvEncoder[R]
//                              ): CsvEncoder[A] =
//  createEncoder(a => enc.encode(gen.to(a)))


implicit def genericEncoder[A, R](
                                   implicit
                                   gen: Generic.Aux[A, R],
                                   env: CsvEncoder[R]
                                 ): CsvEncoder[A] =
  createEncoder(a => env.encode(gen.to(a)))

writeCsv(employees)



