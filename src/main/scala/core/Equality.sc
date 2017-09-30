import core._

val p1 = new Point(1,2)

val p2 = new Point(1,2)

val p3 = new Point(1,3)

p1 == p2

p1 == p3

val cp = new ColorPoint(1,2, AbstractTypes.Color.Red)

cp == p1

p1 == cp

val cpA = new Point(1,1) {
  override val y = 2
}

p1  == cpA

