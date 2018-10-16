import exceptions._
import scala.collection.mutable.{ Queue => MutableQueue }

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
    private var transactions = new MutableQueue[Transaction]

    // Remove and return the first element from the queue
    def pop: Transaction = transactions.synchronized {
      transactions.dequeue()
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = transactions.synchronized {
      transactions.isEmpty
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = transactions.synchronized {
      this.transactions += t
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = transactions.synchronized {
      transactions.front
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = transactions.synchronized {
      transactions.iterator
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

      def doTransaction() = {
        var preFromAmount = 0.0
        var postFromAmount = 0.0
        var preToAmount = 0.0
        var postToAmount = 0.0
        try {
          preFromAmount = from.balance.amount
          from withdraw amount
          postFromAmount = from.balance.amount
          preToAmount = to.balance.amount
          to deposit amount
          postToAmount = from.balance.amount
          this.status = TransactionStatus.SUCCESS
        } catch {
          case e: Exception => {
            if (postFromAmount < preFromAmount) {
              from.balance.amount = preFromAmount
            }
            if (preToAmount > postToAmount) {
              to.balance.amount = postFromAmount
            }
            this.status = TransactionStatus.FAILED
          }
        }
      }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
      } else to synchronized {
          from synchronized {
            doTransaction
          }
      }

      // Extend this method to satisfy requirements.
    }
}
