sealed trait MyList[+T]{
  def ::[B >: T](elem: B): MyList[B]
}

case class MyNil[+T]() extends MyList[T] {
  override def ::[B >: T](elem: B): MyList[B] = MyCons(elem, this)
}

case class MyCons[+T](head: T, tail: MyList[T]) extends MyList[T] {
  override def ::[B >: T](elem: B): MyList[B] = MyCons(elem, this)
}

object MyList {
  def apply[A](a: A*): MyList[A] = if (a.isEmpty) MyNil() else MyCons(a.head, apply(a.tail: _*))
}

object MainX extends App {
  val a = MyList(4).::(4) // = 4 :: MyList(4) this is called infix operator
  println(a)
}