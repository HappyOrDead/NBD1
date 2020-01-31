import scala.annotation.tailrec
import util.control.Breaks._

object Main {

  val _daysOfWeek: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  val _products = Map("clothes" -> 5, "shoes" -> 10)
  val _cars = Map("volvo" -> 1999, "mitsubishi" -> 2000, "hundai" -> 3500)


  def main(args: Array[String]): Unit = {
    println(task1("monday"))
    val b: BankAccount = new BankAccount()
    b.currentBalance
    b.deposits(20.3)
    b.currentBalance
    b.withdraw(10)
    b.currentBalance
    new BankAccount(1000).currentBalance
    val peter: PersonTask3 = new PersonTask3("Peter", "Wang")
    peter.greeting(new PersonTask3("Keksimus", "Maksimus"))
    peter.greeting(new PersonTask3("Jakub", "Magajewski"))
    peter.greeting(new PersonTask3("Shynder", "Illia"))
    println(task4(x => x * x, 2))
    object p1 extends PersonTask5("Keksimus", "Maksimus") with Employee
    p1.salary_(1000)
    p1.taxToPay
    object p2 extends PersonTask5("Keksimus", "Maksimus") with Student
    p2.taxToPay
    object p3 extends PersonTask5("Keksimus", "Maksimus") with Teacher
    p3.salary_(1000)
    p3.taxToPay
    object p4 extends PersonTask5("Keksimus", "Maksimus") with Employee with Student
    p4.salary_(1000)
    p4.taxToPay
    object p5 extends PersonTask5("Keksimus", "Maksimus") with Student with Employee
    p5.salary_(1000)
    p5.taxToPay
  }
  def task1(day: String): String = day match {
    case d if _daysOfWeek.map(_.toLowerCase()).filter(!_.startsWith("s")).contains(d.toLowerCase()) => return "work"
    case d if _daysOfWeek.map(_.toLowerCase()).filter(_.startsWith("s")).contains(d.toLowerCase()) => return "weekends"
    case _ => return "no such day"
  }

  def task4(f: Int => Int, a: Int): Int = f(f(f(a)))

}

class BankAccount(private var balance: Double) {

  def this() { this(0) }

  def deposits(amount: Double): Unit = {
    if (amount > 0) balance = balance + amount
    else throw new Exception("wrong amount input")
  }

  def withdraw(amount: Double): Unit = {
    if (0 < amount && amount <= balance) {
      balance = balance - amount
    } else throw new Exception("wrong amount input")
  }

  def currentBalance: Unit = println("Your current balance is: " + balance)
}

case class PersonTask3(var firstName: String, var lastName: String) {
  def greeting(person: PersonTask3): Unit = person match {
    case PersonTask3("David", _) => println("Hi! David")
    case PersonTask3(_, "Keksimus") => println("Hi! Mr. Keksimus")
    case PersonTask3(fn, ln)     => println(s"Hello! $fn $ln")
  }
}

abstract class PersonTask5(private var firstName: String, private var lastName: String) {
  def taxToPay: Unit
}
trait Employee extends PersonTask5 {
  private var sal: Double = _
  def salary = sal
  def salary_(s: Double): Unit = sal = s
  override def taxToPay: Unit = println("Your tax to pay is: " + sal * 0.2)
}
trait Student extends PersonTask5 {
  override def taxToPay: Unit = println("Your tax to pay is: 0")
}
trait Teacher extends Employee {
  override def taxToPay: Unit = println("Your tax to pay is: " + salary * 0.1)
}

