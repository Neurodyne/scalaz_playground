object appFunctor extends App {
 
  // This is a Functor
  trait Functor[M[_]] {
 
    /* convert f into a function mapping M[A] to M[B]
     * eg. if M were List, and f was Int ⇒ String
     * fmap would yield List[Int] ⇒ List[String]
     */
    def fmap[A, B](f: A => B): M[A] => M[B]
  }
 
  /* Here are a couple of examples for Option and List Functors
   * They are implicit so they can be used below in enrichWithFunctor
   */
 
  implicit object OptionFunctor extends Functor[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] 
    = option => option map f
  }
 
  implicit object ListFunctor extends Functor[List] {
    def fmap[A, B](f: A => B): List[A] => List[B] 
    = list => list map f
  }
 
  /* enrichWithFunctor is an implicit to enrich any kind 
   * with an fmap method.
   * List, Option and any other Foo[X] can be enriched with the
   * new method.
   */
  implicit def enrichWithFunctor[M[_], A](m: M[A]) = new {
 
    /* fmap requires an implicit functor, whose type is M, to which it
     * delegates to do the real work
     */
    def mapWith[B](f: A => B)(implicit functor: Functor[M]): M[B] 
      = functor.fmap(f)(m)
  }
 
  // some examples
 
  println(List(1, 2) mapWith (_ + 1)) // List(2, 3)
 
  println(some(1) mapWith (_ + 1) mapWith (_ * 3)) // Some(6)
 
  println(none[Int] mapWith (_ + 1)) // None
 
  def some[A](a: A): Option[A] = Some(a)
  def none[A]: Option[A] = None
}
