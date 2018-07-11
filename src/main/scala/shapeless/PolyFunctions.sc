import shapeless._

object myPoly extends Poly1 {

  implicit val intCase: Case.Aux[Int, Double] =
    at(num => num / 2.0)

  implicit val stringCase: Case.Aux[String, Int] =
    at(str => str.length)
}

myPoly(1)
myPoly("")

//myPoly(1.0) doesn't compile

object multiply extends Poly2 {

  implicit val intIntCase: Case.Aux[Int, Int, Int] =
    at((a, b) => a * b)

  implicit val intStrCase: Case.Aux[Int, String, String] =
    at((a, b) => b * a)
}
multiply(3, 4)

multiply(3, "4")

///// map
object sizeOf extends Poly1 {

  implicit val intCase: Case.Aux[Int, Int] =
    at(identity)

  implicit val stringCase: Case.Aux[String, Int] =
    at(_.length)

  implicit val booleanCase: Case.Aux[Boolean, Int] =
    at(bool => if(bool) 1 else 0)
}
(10 :: "hello" :: true :: HNil).map(sizeOf)

///// flatMap

object valueAndSizeOf extends Poly1 {

  implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
    at(num => num :: num :: HNil)

  implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
    at(str => str :: str.length :: HNil)

  implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
    at(bool => bool :: (if(bool) 1 else 0) :: HNil)
}
(10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf)

case class IceCream(name: String, numCherries: Int, inCone: Boolean)

val iceCream = IceCream("Sundae", 1, false)

Generic[IceCream].to(iceCream).flatMap(valueAndSizeOf)



