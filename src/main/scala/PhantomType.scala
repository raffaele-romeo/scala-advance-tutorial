sealed trait Status
sealed trait Open   extends Status
sealed trait Closed extends Status

trait Door[S <: Status]

object Door {
  def apply[S <: Status]: Door[S] = new Door[S] {}

  def open[S <: Closed](d: Door[S]): Door[Open]  = Door[Open]
  def close[S <: Open](d: Door[S]): Door[Closed] = Door[Closed]
}

class Container3[A](value: A) {
  def addInt(implicit evidence: A =:= Int): Int = 123 + value
  def open(implicit evidence: A <:< Closed) = Door[Open]
}

object PhantomType {
  def main(args: Array[String]): Unit = {
    (new Container3(123).addInt)
    //(new Container3("123").addInt)
  }
}
