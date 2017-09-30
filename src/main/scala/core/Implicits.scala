package core;

/**
  * Created by serg on 2/7/17.
  */
object Implicits {

  import scala.language.implicitConversions

  implicit def doubleToInt(d:Double):Int = d.toInt

  val i:Int = 3.5

  class A

  class B {
    def doIt():Unit = ()
  }

  implicit def a2b(a:A):B = new B


  new A().doIt


  class PreferredPrompt(val pref:String)

  object Greeter {
    def greet(name:String)(implicit prompt:PreferredPrompt, company:String):Unit = {
      println("Hello " + name + " from " + company)
      println(prompt.pref)
    }
  }

  implicit val bash = new PreferredPrompt(">")

  implicit val company = "Sintez"

  def main(args: Array[String]): Unit = {
    Greeter.greet("Serg")
  }

}
