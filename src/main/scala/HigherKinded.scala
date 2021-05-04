trait Container[M[_]] {
  def put[A](x: A): M[A]
  def get[A](x: M[A]): A
}

object Container {
  implicit val listContainer: Container[List] = new Container[List] {
    override def put[A](x: A)          = List(x)
    override def get[A](m: List[A]): A = m.head
  }

  implicit val optionContain: Container[Option] = new Container[Option] {
    override def put[A](x: A): Option[A] = Some(x)

    override def get[A](x: Option[A]): A = x.get
  }

  def tupleize[M[_], A, B](first: M[A], second: M[B])(
      implicit M: Container[M]): M[(A, B)] = {
    M.put(M.get(first), M.get(second))
  }
}

object HigherKinded extends App {
  import Container._

  implicitly[Container[List]].put("ciao")
  implicitly[Container[List]].put(200)

  tupleize(List(1, 2, 3), List(4, 5, 6))

}
