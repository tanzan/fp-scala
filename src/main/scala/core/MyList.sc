import core._

1::2::3::MyNil
(1::2::3::MyNil).reverse

(1::2::MyNil):::(3::4::MyNil)
MyList(1,2,3)
MyList(1,2,3).drop(2)
MyList(1,2,3).map(_ * 2)
MyList(1,2,3).flatMap(x => MyList(x * x))
MyList(1,2,3,4).dropRight(2)
MyList(1,2,3,4).take(2)


MyList(1,2,3) match {
  case MyList(a,b,_ *) => (a,b)
}