import scala.annotation.tailrec
import util.control.Breaks._

object Main {

  val _daysOfWeek: List[String] = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  val _products = Map("clothes" -> 5, "shoes" -> 10)
  val _cars = Map("volvo" -> 1999, "mitsubishi" -> 2000, "hundai" -> 3500)

  //1
  def main(args: Array[String]): Unit = {
    println(ListToStringSimpleFor(_daysOfWeek))
    println(ListToStringOnlySDays(_daysOfWeek))
    println(ListToStringWhileLoop(_daysOfWeek))
    ListToStringRecursive()
    ListToStringRecursiveReverse()
    ListToStringFoldl()
    ListToStringFoldr()
    ListToStringFoldlOnlyS()
    task5();
    println(task6(1 :: 2 :: 3 :: Nil))
    println(task7(-11 :: -2.4 :: 3.0 :: 20.3 :: Nil))
    task8(Tuple3(1, 300.2, "kek"))
    println(task9(-1.3 :: 2.1 :: 0.0 :: 3.4 :: 0.0 :: Nil))
    task10("volvo")
    task10("mitsubishi")
  }

  def ListToStringSimpleFor ( daysOfWeek : List[String] ): String ={
    var returnString = ""
    for (day <- daysOfWeek){
      returnString += day + ", "
    }
    return  returnString.substring(0, returnString.length-2)
  }

  def ListToStringOnlySDays ( daysOfWeek : List[String] ): String ={
    var returnString = ""
    for (day <- daysOfWeek){

      breakable {
        if(!day.startsWith("S"))
          break
        returnString += day + ", "
      }

    }
    return  returnString.substring(0, returnString.length-2)
  }

  def ListToStringWhileLoop ( daysOfWeek : List[String] ): String ={
    var returnString = ""
    var listLength = daysOfWeek.length
    var count = 0
    while(count < listLength) {
      returnString += daysOfWeek(count) + ", "
      count += 1
    }
    return  returnString.substring(0, returnString.length-2)
  }

  //2
  def ListToStringRecursive(index : Int = 0, result : String = ""): Unit ={
    if(index >= _daysOfWeek.length){
      println(result)
      return
    }
    var kek = result +_daysOfWeek(index) + ", "
    ListToStringRecursive(index +1, kek)
  }

  def ListToStringRecursiveReverse(index : Int = _daysOfWeek.length -1, result : String = ""): Unit ={
    if(index <= -1){
      println(result)
      return
    }
    var kek = result +_daysOfWeek(index) + ", "
    ListToStringRecursiveReverse(index - 1, kek)
  }

  //3
  @tailrec
  def ListToStringTailRec(index : Int = 0, result : String = ""): Unit ={
    if(index >= _daysOfWeek.length){
      println(result)
      return
    }
    ListToStringTailRec(index +1, result +_daysOfWeek(index) + ", ")
  }

  //4
  def ListToStringFoldl(): Unit ={
    var result: String = ""
    result = _daysOfWeek.foldLeft("")(_ + _ + ", ")
	  println(result.substring(0, result.length - 2))
  }

  def ListToStringFoldr(): Unit ={
    var result: String = ""
    result = _daysOfWeek.foldRight("")(_ + ", " + _)
	  println(result.substring(0, result.length - 2))
  }

  def ListToStringFoldlOnlyS(): Unit ={
    var result: String = ""
    result = _daysOfWeek.foldRight("")( (next, sum) => if (next.toLowerCase.startsWith("s")) next + ", " + sum else sum )
	  println(result.substring(0, result.length - 2))
  }
  
  //5

  def task5(): Unit ={
    println(_products.mapValues(_ * 0.9).toMap)
  }

  def task6(l: List[Int]): List[Int] = l.map(_ + 1)

  def task7(l: List[Double]): List[Double] = l.filter(-5 < _).filter(_ < 12).map(_.abs)

  def task8(tuple: Tuple3[Int, Double, String]): Unit = println(tuple)

  def task9(l: List[Double]): List[Double] = {
    if (l.isEmpty) l
    else if (l.head == 0) task9(l.tail)
    else l.head :: task9(l.tail)
  }

  def task10(car: String) = {
    val year: Option[Int] = _cars.get(car.toLowerCase)
    println(year.getOrElse("Sorry, we dont have this car: " + car))
    if (year.isDefined && year.get >= 2000)
      println(year + " is too old, its year 3501 now!")
  }
}
