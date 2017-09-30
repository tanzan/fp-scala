import shapeless.the

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

// Custom data type:
case class Employee(name: String, number: Int, manager: Boolean)
// CsvEncoder instance for the custom data type:
implicit val employeeEncoder: CsvEncoder[Employee] =
  new CsvEncoder[Employee] {
    def encode(e: Employee): List[String] =
      List(
        e.name,
        e.number.toString,
        if(e.manager) "yes" else "no"
      )
  }

def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  values.map(value => enc.encode(value).mkString(",")).mkString("\n")

val employees: List[Employee] = List(
  Employee("Bill", 1, true),
  Employee("Peter", 2, false),
  Employee("Milton", 3, false)
)

writeCsv(employees)


case class IceCream(name: String, numCherries: Int, inCone: Boolean)

implicit val iceCreamEncoder: CsvEncoder[IceCream] =
  new CsvEncoder[IceCream] {
    def encode(i: IceCream): List[String] =
      List(
        i.name,
        i.numCherries.toString,
        if(i.inCone) "yes" else "no"
      )
  }

val iceCreams: List[IceCream] = List(
  IceCream("Sundae", 1, false),
  IceCream("Cornetto", 0, true),
  IceCream("Banana Split", 0, false)
)
writeCsv(iceCreams)


implicit def pairEncoder[A, B](
                                implicit
                                aEncoder: CsvEncoder[A],
                                bEncoder: CsvEncoder[B]
                              ): CsvEncoder[(A, B)] =
  new CsvEncoder[(A, B)] {
    def encode(pair: (A, B)): List[String] = {
      val (a, b) = pair
      aEncoder.encode(a) ++ bEncoder.encode(b)
    }
  }

writeCsv(employees zip iceCreams)

/////

//Idiomatic type class definition

object CsvEncoder {
  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc

  // "Constructor" method
  def instance[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

  // Globally visible type class instances

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    instance(b => if(b) List("yes") else List("no"))
}

CsvEncoder[IceCream]
implicitly[CsvEncoder[IceCream]]
the[CsvEncoder[IceCream]]