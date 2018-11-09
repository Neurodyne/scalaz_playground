import scalaz._
import Scalaz._
 
object appWriterMonad extends App {
 
  // the left side can be any monoid. E.g something which support
  // concatenation and has an empty function: e.g. String, List, Set etc.
  type Result[T] = Writer[List[String], T]
 
  def doSomeAction() : Result[Int] = {
    // do the calculation to get a specific result
    val res = 10
    // create a writer by using set
    res.set(List(s"Doing some action and returning res"))
  }
 
  def doingAnotherAction(b: Int) : Result[Int] = {
    // do the calculation to get a specific result
    val res = b * 2
    // create a writer by using set
    res.set(List(s"Doing another action and multiplying $b with 2"))
  }
 
  def andTheFinalAction(b: Int) : Result[String] = {
    val res = s"bb:$b:bb"
 
    // create a writer by using set
    res.set(List(s"Final action is setting $b to a string"))
  }
 
  // returns a tuple (List, Int)
  println(doSomeAction().run)
 
  val combined = for {
    a <- doSomeAction()
    b <- doingAnotherAction(a)
    c <- andTheFinalAction(b)
  } yield c
 
  // Returns a tuple: (List, String)
  println(combined.run)
}
