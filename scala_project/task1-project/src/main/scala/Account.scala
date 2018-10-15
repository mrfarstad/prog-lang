import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = balance.synchronized {
        this.balance.amount -= amount
    }
    def deposit(amount: Double): Unit = balance.synchronized {
        this.balance.amount += amount
    }
    def getBalanceAmount: Double = balance.synchronized {
        this.balance.amount
    }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
