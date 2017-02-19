/**
  * Created by serg on 2/19/17.
  */
class Point(val x:Int,val y:Int) {

  override def hashCode(): Int = (x, y)##

  override def equals(obj: Any): Boolean = obj match {
    case that:Point =>
      that.canEqual(this) && this.x == that.x && this.y == that.y
    case _ => false
  }

  protected def canEqual(obj: Any):Boolean = obj.isInstanceOf[Point]
}

import AbstractTypes._

class ColorPoint(x:Int, y:Int, val color: Color.Value) extends Point(x,y) {

  override def equals(obj: Any): Boolean = obj match {
    case that:ColorPoint => super.equals(that) && color == that.color
    case _ => false
  }

  override protected def canEqual(obj: Any): Boolean = obj.isInstanceOf[ColorPoint]
}