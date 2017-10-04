///// Singleton type
object Foo

val f:Foo.type = Foo

class A

val a = new A

val b:a.type = a

// val c:a.type = new A doesn't compile


///// Literal types

import shapeless.syntax.singleton._

var x = 41.narrow

// x = 42 doesn't compile

//// Phantom types

val number = 42

trait Cherries

val numCherries = number.asInstanceOf[Int with Cherries]

import shapeless.labelled.{KeyTag, FieldType}

val someNumber = 123
val numCherries2 = "numCherries" ->> someNumber

import shapeless.labelled.field
field[Cherries](123)

import shapeless.Witness

def getFieldName[K, V](value: FieldType[K, V])
                      (implicit witness: Witness.Aux[K]): K =
  witness.value

getFieldName(numCherries2)

def getFieldValue[K, V](value: FieldType[K, V]): V = value

getFieldValue(numCherries2)


