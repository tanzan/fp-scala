import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr}

case class Red()
case class Amber()
case class Green()

type Light = Red :+: Amber :+: Green :+: CNil

val red: Light = Inl(Red())
val green: Light = Inr(Inr(Inl(Green())))

/////

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

val gen = Generic[Shape]

gen.to(Rectangle(3.0, 4.0))

gen.to(Circle(1.0))




