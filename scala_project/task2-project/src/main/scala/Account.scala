import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        (transactions values) toList
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed
        !getTransactions.exists(t => t.status != TransactionStatus.SUCCESS)
    }

    def withdraw(amount: Double): Unit = balance.synchronized {
        if (amount < 0) throw new IllegalAmountException("Negative withdrawal amounts is not allowed")
        else if (amount > balance.amount) throw new NoSufficientFundsException("Insuficcient founds")
        balance.amount -= amount
    }

    def deposit(amount: Double): Unit = balance.synchronized {
        if (amount < 0) throw new IllegalAmountException("Negative deposit amounts is not allowed")
        balance.amount += amount
    }

    def getBalanceAmount: Double = balance.synchronized {
        balance.amount
    }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        transactions += (t.id -> t)
        BankManager.findBank(bankId) ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
		case IdentifyActor => sender ! this

		case TransactionRequestReceipt(to, transactionId, transaction) => {
      if (to == accountId) {
        transactions -= transactionId
        transactions += (transactionId -> transaction)
      }
		}

		case BalanceRequest => ??? // Should return current balance

		case t: Transaction => {
			// Handle incoming transaction
      if (t.status == TransactionStatus.PENDING) {
          try {
              deposit(t.amount)
              t.status = TransactionStatus.SUCCESS
          } catch {
              case e: IllegalAmountException =>
                  t.status = TransactionStatus.FAILED
          }
      }
      var returnBankId = ""
      var returnAccountId = ""
      if (t.to.length <= 4) {
          returnBankId = bankId
          returnAccountId = t.to
      } else {
          returnBankId = t.to.substring(0,4)
          returnAccountId = t.to.substring(4)
      }
      BankManager.findBank(returnBankId) ! TransactionRequestReceipt(returnAccountId, t.id, t)
		}

		case msg => ???
    }


}
