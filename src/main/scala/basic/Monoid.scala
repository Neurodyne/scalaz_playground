package prop_pkg {

trait Monoid[A] {
  def mappend (a1:A, a2:A): A 
  def mzero: A
}


object Monoid {
  implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend (a:Int, b:Int):Int = a + b 
    def mzero: Int = 0
  }
}

class Property (a:Boolean, b:Boolean) {
  def |-> = a && b 
  def |=> = a && b

  assert (a &&b)
}

}


object appMonoid extends App {

  import prop_pkg._

  def sum[A:Monoid](xs:List[A]):A = {
    val m  = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.mappend)
  }
  
  val res = sum(List(1,2,3,4))
  println (s"$res")

  val a = true
  val b = true
  
  val p  = new Property(a,b)
  

  (1 to 5) foreach { clk =>
    println (s"Tick")
  }
  
}


