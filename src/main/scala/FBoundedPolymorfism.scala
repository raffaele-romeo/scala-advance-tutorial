
//Often it’s necessary to access a concrete subclass in a (generic) trait
trait Container2[A <: Container2[A]] extends Ordered[A]{
 override def compare(that: A): Int
}

class MyContainer extends Container2[MyContainer]{
  override def compare(that: MyContainer): Int = 0
}
