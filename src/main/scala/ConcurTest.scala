
/*
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
// Case Classes for the Messages
case class Deposit(amount: Int)
case class Withdrawal(amount: Int)
case class Reset()
case class Total()

// Actor Definition
object BankAccount extends Actor {
  def act = {
    var sum = 0
    loop {
      receive {
	case Deposit(n) => sum += n
	case Withdrawal(n) => sum -= n
	case Reset => sum = 0
	case Total => reply(sum); exit
      }
    }
  }
}

// Accounting Simulation, Showing Consistent, Coherent Computation
object Accounting extends App {
  val r = new Random()
  // Create lists of Withdrawal and Deposit amounts
  var withdrawals = new ArrayBuffer[Int]()
  var deposits    = new ArrayBuffer[Int]()
  for (i <- (1 to 20))  withdrawals += i
  for (i <- (1 to 100)) deposits += i
  var amount = 0
  BankAccount.start
  while(deposits.length > 0) {
    amount = deposits.remove(r.nextInt(deposits.length))
    BankAccount ! Deposit(amount)
  }

  while(withdrawals.length > 0) {
    amount = withdrawals.remove(r.nextInt(withdrawals.length))
    BankAccount ! Withdrawal(amount)
  }
  BankAccount !? Total match {
    case result: Int => println(result)
  }
}
*/
