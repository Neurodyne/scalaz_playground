import scalaz._
import Scalaz._
 
object appApplicative extends App {
 
  /*
   * Lets assume some Options which must be applied to a function.
   */
  val x:Option[Int] = 2.some // scalaz enrichment for options
  val y:Option[Int] = 3.some
  val z:Option[Int] = 5.some
 
  // Lets add them without applicative functors
  val usingFor = for (theX <- x; theY <- y; theZ <- z) 
                   yield theX + theY + theZ
 
  val usingMaps = x flatMap 
                    (theX => y flatMap 
                      (theY => z map 
                        (theZ => theZ + theY + theX)))
 
  /* With scalaz we can do the following instead of for or maps
   * First we need to put the function in the right form, curried.
   * To understand why please read the references I've given below.
   */
  val addInts = ( (a:Int, b:Int, c:Int) => a + b + c ).curried
 
  // apply the function to x, y and z
  val sum = x <*> (y <*> (z map addInts)) // Some(10)
 
  // Scalaz offers an alternative syntax that is easier to use
  (x |@| y |@| z) {_ + _ + _}   // Some(10)
 
  /*
   * If one of the options is a none
   * then the result of the whole expression will be none.
   */
  (x |@| none[Int]) {_ + _} // None
 
  /*
   * The function can be any method, including 'apply'
   */
  case class Person(age: Int, height:Double, name: String)
 
  /*
   * Person.apply method is a function (Int, Double, String) => Person
   */
  (some(2) |@| some(1.1) |@| none[String]) {Person.apply _}  
  // none
 
  (some(4) |@| some(1.1) |@| "Angelica".some) {Person.apply _} 
  // Some(Person(4, 1.1, Angelica))
 
  /*
   * The beauty of this is that it works with ANY higher kind, eg. List
   * or your own types!
   */
  val l1 = 1 :: 2 :: Nil
  val l2 = 3 :: 4 :: Nil
 
  (l1 |@| l2) {_ + _} // List(4, 5, 5, 6)
  //(<xml/> |@| <xml2/>){_ ++ _} // List(<xml></xml><xml2></xml2>)
}

