
object AUXPattern extends App {


  trait Foo[A] {
    type B   //Abstract type
    def value: B
  }

  object Foo {
    type Aux[A0, B0] = Foo[A0] { type B = B0 }

    implicit def fi: Aux[Int, String] = new Foo[Int] {
      type B = String
      val value = "Foo"
    }
    implicit def fs: Aux[String, Boolean] = new Foo[String] {
      type B = Boolean
      val value = false
    }

  }

  def ciao[T, R, Z](t: T)(implicit f: Foo.Aux[T, R], m: Foo.Aux[R, Z]): R =
    f.value
  val res = ciao(2)
  println(s"res: ${res}")
}
