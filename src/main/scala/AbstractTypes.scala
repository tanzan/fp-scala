/**
  * Created by serg on 2/6/17.
  */
object AbstractTypes {

  trait Abstract {
    type T
    val initial:T
    protected var current:T
  }

  class Concrete extends Abstract {
    type T = String
    val initial: String  = ""
    var current:String = initial
  }

  class Food

  abstract class Animal {
    type SuitableFood <: Food
    def eat(food: SuitableFood)
  }

  class Grass extends Food

  class Cow extends Animal {
    type SuitableFood = Grass
    def eat(food: Grass) {} // This won’t compile
  }

  class Fish extends Food

  class DogFood extends Food

  class Dog extends Animal {
    type SuitableFood = DogFood
    override def eat(food: DogFood) {}
  }

  class Pasture {
    var animals: List[Animal { type SuitableFood = Grass }] = Nil
    // ...
    animals.foreach(_.eat(new Grass))
  }

  class Outer {
    class Inner
  }


  object Color extends Enumeration {
    val Red = Value
    val Green = Value
    val Blue = Value
  }


  abstract class CurrencyZone {
    type Currency <: AbstractCurrency
    def make(units:Long):Currency

    val CurrencyUnit:Currency
    val destination: String

    def from(that:CurrencyZone#AbstractCurrency):Currency =
      make(math.round(that.amount.toDouble * Converter.exchangeRate(that.zone.destination)(destination)))

    abstract class AbstractCurrency {
      val zone:CurrencyZone = CurrencyZone.this

      val amount: Long

      private def decimals(n: Long):Int =
        if (n <= 1) 0 else 1 + decimals(n/10)

      override def toString() =
        (amount.toDouble/CurrencyUnit.amount).formatted("%." + decimals(CurrencyUnit.amount) + "f") + " " + destination

      def +(that: Currency): Currency = make(amount + that.amount)

      def *(factor: Double): Currency = make(math.round(amount * factor))
    }
  }


  object US extends CurrencyZone {
    type Currency = Dollar

    val destination = "USD"

    abstract class Dollar extends AbstractCurrency

    def make(units: Long) = new Dollar {
      val amount: Long = units
    }

    val Cent = make(1)
    val Dollar = make(100)

    val CurrencyUnit = Dollar
  }

  object UA extends CurrencyZone {
    type Currency = Hryvna

    val destination = "UAH"

    abstract class Hryvna extends AbstractCurrency

    def make(units: Long) = new Hryvna {
      val amount: Long = units
    }

    val Kopiyka = make(1)
    val Hryvna = make(100)

    val CurrencyUnit = Hryvna
  }

  object Converter {
    var exchangeRate:Map[String,Map[String, Double]] = Map(
      "USD" -> Map("USD" -> 1.0   , "EUR" -> 0.7596,
        "JPY" -> 1.211 , "CHF" -> 1.223, "UAH" -> 27),
      "EUR" -> Map("USD" -> 1.316 , "EUR" -> 1.0   ,
        "JPY" -> 1.594 , "CHF" -> 1.623),
      "JPY" -> Map("USD" -> 0.8257, "EUR" -> 0.6272,
        "JPY" -> 1.0   , "CHF" -> 1.018),
      "CHF" -> Map("USD" -> 0.8108, "EUR" -> 0.6160,
        "JPY" -> 0.982 , "CHF" -> 1.0  ),
      "UAH" -> Map("USD" -> 1/27.0)
    ) }


}

object AbstractTypesMain extends App {
  import AbstractTypes._
  val c:Concrete = new Concrete
  val a:Abstract = c

  val bessy = new Cow
  bessy.eat(new Grass)

  val lessy = new Dog


  val bootsie = new Dog

  bootsie.eat(new lessy.SuitableFood)



  val o1 = new Outer
  val o2 = new Outer

  val t:o1.type  =  o1

  val i:Outer#Inner = new o2.Inner
  //val i1:o1.Inner = new o2.Inner does not compile

  val f:lessy.SuitableFood = new bootsie.SuitableFood

  val r:Color.Value= Color.Red

  println(US.make(100))

  println(UA.make(2700))
  println(UA.from(US.make(100)))
}
