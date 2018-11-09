/**
 * Functions that do the flattening
 */
object Loans {
 
  def loan[T1, T2](f: (T1, T2) => Unit)(implicit f1: (T1 => Unit) => Unit, f2: (T2 => Unit) => Unit)
  = f1 { a => f2 { b => f(a, b) } }
 
  def loan[T1, T2, T3](f: (T1, T2, T3) => Unit)(implicit f1: (T1 => Unit) => Unit, f2: (T2 => Unit) => Unit, f3: (T3 => Unit) => Unit) 
  = f1 { a => f2 { b => f3 { c => f(a, b, c) } } }
 
}
 
object appLoan extends App {
 
  import Loans._
 
  // create some fake types for the purpose of illustration
  type Connection = Unit
  type Session = Int
  type User = String
 
  // implicits calling functions with loans
  implicit def withDatabase(f: Connection => Unit) = f(1)
  implicit def withSession(f: Session => Unit) = f(1)
  implicit def withUser(f: User => Unit) = f("Bob")
 
  // conventional loan pattern results in deep nesting
  withDatabase { connection =>
    withSession { session =>
      withUser { user =>
        println("Hi from the depths.")
      }
    }
  }
 
  // flattened
  loan { (c: Connection, s: Session, u: User) =>
    println("hi from flatland") 
  }
 
}
