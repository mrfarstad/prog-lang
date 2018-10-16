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
        try {
          from withdraw amount
          to deposit amount
          this.status = TransactionStatus.SUCCESS
        } catch {
          case e: Exception => this.status = TransactionStatus.FAILED
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
