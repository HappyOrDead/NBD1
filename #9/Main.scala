import scala.annotation.tailrec
import util.control.Breaks._

object Main2 extends App {

  //Task 1
  class Container[A](value: A) {
    private var _value: A = value

    def getContent: A = _value
    def applyFunction(func: A => A): A = {
      _value = func(_value)
      return _value
    }

  }

  //Task2
  trait Maybe[A]

  class Yes[A](value: A) extends Maybe[A] {
    private var pValue: A = value

    def getContent: A = pValue
  }

  class No extends Maybe[Nothing]

  //Task 3
  class Task3[A](value: A) {
    private var _value: A = value

    def getContent: A = _value

    def applyFunction(func: A => A): A = {
      func(_value) match {
        case _: No => _value
        case _: Yes[_] =>
          _value = func(_value)
          _value
        case _ => null.asInstanceOf[A]
      }
    }
  }

  class Task4[A](value: A) {
    private var _value: A = value
    def getContent: A = _value
    def getOrElse[B]: B =
      _value match {
        case _: No => "It is empty!".asInstanceOf[B]
        case _: Yes[_] => _value.asInstanceOf[Yes[A]].getContent.asInstanceOf[B]
        case _ => null.asInstanceOf[B]
      }
  }

  //Task 1
  println("Task 1:")
  val task1: Container[String] = new Container[String]("test")
  task1.applyFunction(a => a + " " + a)
  println(task1.getContent)
  //Task 2
  println("Task 2:")
  var no: No = new No()
  println(no.isInstanceOf[Maybe[_]])
  var yes: Yes[String] = new Yes("a")
  println(yes.isInstanceOf[Maybe[_]])

  //Task 3
  println("Task3:")
  val no3: Task3[No] = new Task3[No](new No())
  no3.applyFunction(a => a)
  println(no3.getContent)
  val yes3: Task3[Yes[String]] = new Task3[Yes[String]](new Yes("test"))
  yes3.applyFunction(a => new Yes(a.getContent + a.getContent))
  println(yes3.getContent.getContent)


  //Task4
  println("Task4:")
  val no4: Task4[No] = new Task4[No](new No())
  println(no4.getOrElse)
  val yes4: Task4[Yes[String]] = new Task4[Yes[String]](new Yes("test"))
  println(yes4.getOrElse)
}

