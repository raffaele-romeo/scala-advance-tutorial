
trait Animal {
  def sound = "rustle"
}

class Bird extends Animal{
  override def sound: String = "pio"
}

class Chicken extends Bird {
  override def sound: String = "qua"
}

object Variance {

  val chicken1: Animal = new Chicken
  val func: Chicken => Animal = (b: Animal) => new Chicken

  //BOUNDS
  def toSound[T <: Animal](list: Seq[T]): Seq[String] = list.map(_.sound)
  def toSound2(list: Seq[_ <: Animal]): Seq[String] = list map (_.sound)
}
