import TypeClass.Addition

object TypeClass {
  type BinaryOp[T] = (T, T) => T

  trait Addition[T] extends BinaryOp[T]
  trait Subtraction[T] extends BinaryOp[T]
  trait Multiplication[T] extends BinaryOp[T]
  trait Division[T] extends BinaryOp[T]

  def add[T](left: T, right: T)(implicit op: Addition[T]): T = {
    op.apply(left, right)
  }

  def sub[T](left: T, right: T)(implicit op: Subtraction[T]): T = {
    op.apply(left, right)
  }
}

object TypeClassImpl {
  //Implicits for Int
  implicit val intAddition = new Addition[Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }
}
