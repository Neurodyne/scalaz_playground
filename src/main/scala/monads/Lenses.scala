import scalaz._
import Scalaz._
 
object appLenses extends App {
 
  // crappy case model, lack of creativity
  case class Account(userName: String, person: Person)
  case class Person(firstName: String, lastName: String, address: List[Address], gender: Gender)
  case class Gender(gender: String)
  case class Address(street: String, number: Int, postalCode: PostalCode)
  case class PostalCode(numberPart: Int, textPart: String)
 
  val acc1 = Account("user123", Person("Jos", "Dirksen",
                List(Address("Street", 1, PostalCode(12,"ABC")),
                     Address("Another", 2, PostalCode(21,"CDE"))),
                Gender("male")))
 
 
  val acc2 = Account("user345", Person("Brigitte", "Rampelt",
                List(Address("Blaat", 31, PostalCode(67,"DEF")),
                     Address("Foo", 12, PostalCode(45,"GHI"))),
                Gender("female")))
 
 
  // when you now want to change something, say change the gender (just because we can) we need to start copying stuff
  val acc1Copy = acc1.copy(
    person = acc1.person.copy(
      gender = Gender("something")
    )
  )

  val genderLens = Lens.lensu[Account, Gender](
     (account, gender) => account.copy(person = account.person.copy(gender = gender)),
     (account) => account.person.gender
   )
   
   // and with a lens we can now directly get the gender
   val updated = genderLens.set(acc1, Gender("Blaat"))
   println(updated)

  // we can use our base lens to create a modify lens
  val toBlaBlaLens = genderLens =>= (_ => Gender("blabla"))
  println(toBlaBlaLens(acc1))
  
  val existingGender = genderLens.get(acc1)
  println(existingGender)

  // First create a lens that returns a person
  val personLens = Lens.lensu[Account, Person](
    (account, person) => account.copy(person = person),
    (account) => account.person
  )
  
  // get the person lastname
  val lastNameLens = Lens.lensu[Person, String](
    (person, lastName) => person.copy(lastName = lastName),
    (person) => person.lastName
  )
  
  // Get the person, then get the lastname, and then set the lastname to
  // new lastname
  val combined = (personLens >=> lastNameLens) =>= (_ => "New LastName")
  
  println(combined(acc1))
}
