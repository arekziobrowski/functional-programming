//Arkadiusz Ziobrowski 229728

//Zadanie 1
class Pair[A, B](var fst : A, var snd : B) {
  override def toString: String = "(" + fst + "," + snd + ")"
}

val p = new Pair[Int, Int](1, 1)

p.toString == "(1,1)"

p.fst = 5
p.snd = 10

p.fst == 5
p.snd == 10
p.toString == "(5,10)"

//Zadanie 2
class BankAccount(initialBalance : Double) {
  private var balance = initialBalance
  def checkBalance = balance
  def deposit(amount : Double) = { balance += amount; balance}
  def withdraw(amount : Double) = { balance -= amount; balance}
}

//a

class CheckingAccount(initialBalance : Double) extends BankAccount(initialBalance) {

  override def deposit(amount: Double): Double = super.deposit(amount - 1)
  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
}

val checkingAccount = new CheckingAccount(100)

checkingAccount.checkBalance == 100

checkingAccount.deposit(1)
checkingAccount.checkBalance == 100

checkingAccount.withdraw(1)
checkingAccount.checkBalance == 98

//b
class SavingsAccount(initialBalance : Double) extends BankAccount(initialBalance) {

  private[this] var numberOfTransactions : Integer = 0

  def earnMonthlyInterest() = {
    super.deposit(super.checkBalance * 0.02)
    numberOfTransactions = 0
  }

  override def deposit(amount: Double): Double = {
    numberOfTransactions += 1

    super.deposit(if(numberOfTransactions > 3) amount - 1 else amount)
  }

  override def withdraw(amount: Double): Double = {
    numberOfTransactions += 1

    super.withdraw(if(numberOfTransactions > 3) amount + 1 else amount)
  }
}

val savingsAccount = new SavingsAccount(100)

savingsAccount.checkBalance == 100

savingsAccount.deposit(1)
savingsAccount.checkBalance == 101

savingsAccount.withdraw(1)
savingsAccount.checkBalance == 100

savingsAccount.deposit(100)
savingsAccount.checkBalance == 200

savingsAccount.withdraw(1)
savingsAccount.checkBalance == 198

savingsAccount.deposit(3)
savingsAccount.checkBalance == 200

savingsAccount.earnMonthlyInterest()
savingsAccount.checkBalance == 204


//Zadanie 3

//a
abstract class Zwierz(val imie : String = "bez imienia") {

  def rodzaj() : String = getClass.getName
  def dajGlos() : String

  override def toString: String = rodzaj() + " " + imie + " daje głos " + dajGlos() + "!"
}

//b

class Pies(imie : String = "bez imienia") extends Zwierz(imie) {

  override def dajGlos() : String = "Hau hau"
}

class Kot(imie : String = "bez imienia") extends Zwierz(imie) {

  override def dajGlos(): String = "Miau miau"
}

val pies = new Pies()
pies.toString

val kot = new Kot("Mruczek")
kot.toString

//c
object TestZwierz {
  def main(args : Array[String]) = {
    Vector(new Kot("Mruczek"), new Pies()).foreach(print)
  }
}

TestZwierz.main(Array())


