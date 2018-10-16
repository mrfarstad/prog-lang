import exceptions.IllegalAmountException

import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

object Bank {
    private var uidCount = 0
    def generateBankId(): Int = this.synchronized {
        val freshUid = uidCount + 1
        uidCount = freshUid
        freshUid
    }
}

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = Bank.generateBankId()
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = ExecutionContext.global

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = transactionsQueue.synchronized {
      transactionsQueue push new Transaction(
        transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
      processTransactions
    }

    // Hint: use a counter
    var uidCount = 0
    def generateAccountId: Int = this.synchronized {
        val freshUid = uidCount + 1
        uidCount = freshUid
        freshUid
    }

    private def processTransactions: Unit = this.synchronized {
//      var attempt = 1
      val t = transactionsQueue.pop
//      do {
        executorContext.execute(t)
//        attempt += 1
//      } while (attempt < allowedAttempts && t.status != TransactionStatus.SUCCESS)
      processedTransactions.push(t)
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = processedTransactions.synchronized {
        processedTransactions.iterator.toList
    }

}
