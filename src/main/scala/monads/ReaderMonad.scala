
import scalaz._
import Scalaz._

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class Account()
case class Statement(acc:Account)
case class Amount(vol:Int, name:String)

class Session {
  def doSomething:Unit = {}
}



object appReaderMonad extends App {

  // introduce a Action type. This represents an action our service can execute. As you can see in
  // the declaration, this Action, requires a Session.
  type OptionTF[A] = OptionT[Future, A]
  type Action[A] = ReaderT[OptionTF, Session, A]
   
  trait AccountService {
    // return an account, or return none when account can't be found
    def getAccount(accountNumber: String) : Action[Account]
    // return the balance when account is opened, or none when it isn't opened yet
    def getBalance(account: Account) :Action[Amount]
    // withdraw an amount from the account, and return the new amount
    def withdraw(account: Account, amount: Amount) : Action[Amount]
    // we can also get an account overview statement, which somehow isn't async
    def getStatement(account: Account): Action[Statement]
  }
   
  /**
    * Normally you would wrap an existing service, with a readerT specific one, which would handle
    * all the conversion stuff.
    */
  object Accounts extends AccountService {
    override def getAccount(accountNumber: String): Action[Account] = ReaderT((session: Session) => {
      // do something with session here, and return result
      session.doSomething
      // Assume we get a Future[Option[Account]]
      val result = Future(Option(Account()))
   
      // and we need to lift it in the OptionTF and return it.
      val asOptionTF: OptionTF[Account] = OptionT(result)
      asOptionTF
    })
   
    override def getBalance(account: Account): Action[Amount] = ReaderT((session: Session) => {
      // do something with session here, and return result
      session.doSomething
      // assume we get a Future[Option[Amount]]
      val result = Future(some(Amount(10,"Dollar")))
      // convert it to the Action type, with explicit type to make compiler happy
      val asOptionTF: OptionTF[Amount] = OptionT(result)
      asOptionTF
    })
   
    override def withdraw(account: Account, amount: Amount): Action[Amount] = ReaderT((session: Session) => {
      // do something with session here, and return result
      session.doSomething
      // assume we get a Future[Amount]
      val result = Future(Amount(5, "Dollar"))
      // convert it to the correct type
      val asOptionTF: OptionTF[Amount] = OptionT(result.map(some(_)))
      asOptionTF
    })
   
    override def getStatement(account: Account): Action[Statement] = ReaderT((session: Session) => {
      // do something with session here, and return result
      session.doSomething
      // assume we get a Statement
      val result = Statement(account)
      // convert it to the correct type
      result.point[OptionTF]
    })
  }
   
  def withdrawWithReaderT(accountNumber: String) = {
    for {
      account <- Accounts.getAccount(accountNumber)
      balance <- Accounts.getBalance(account)
      _ <- Accounts.withdraw(account, balance)
      statement <- Accounts.getStatement(account)
    } yield statement
  }
   
  // this is the result wrapped in the option
  val finalResult = withdrawWithReaderT("1234").run(new Session)
  // get the Future[Option] and wait for the result
  println(Await.result(finalResult.run, 5 seconds))

}
