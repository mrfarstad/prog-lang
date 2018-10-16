import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = balance.synchronized {
        if (amount < 0) throw new IllegalAmountException("Negative withdrawal amounts is not allowed")
        else if (amount > this.balance.amount) throw new NoSufficientFundsException("Insufficient funds")
        this.balance.amount -= amount
    }
    def deposit(amount: Double): Unit = balance.synchronized {
        if (amount < 0) throw new IllegalAmountException("Negative deposit amounts is not allowed")
        this.balance.amount += amount
    }
    def getBalanceAmount: Double = balance.synchronized {
        this.balance.amount
    }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
