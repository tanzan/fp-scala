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

}

object AbstractTypesMain {
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

  val i:Outer#Inner = new o2.Inner
  //val i1:o1.Inner = new o2.Inner does not compile

  val f:lessy.SuitableFood = new bootsie.SuitableFood

  val r:Color.Value = Color.Red
}
