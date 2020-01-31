
object Main2 extends App {

  class Maybe[A](var value: A) {
    def map[B](f: A => B): Maybe[B] = {
      val newValue = f(value)
      new Maybe(newValue)
    }
    def flatMap[B](f: A => Maybe[B]): Maybe[B] = {
      val newValue = f(value)
      newValue
    }
    override def toString: String = value.toString
  }
  class Yes[A](value: A) extends Maybe[A](value) {
    private var v: A = value
    def getContent: A = v
  }
  object Maybe {
    def apply[A](value: A): Maybe[A] = new Maybe(value)
  }
  object Yes {
    def apply[A](value: A): Yes[A] = new Yes(value)
  }

  println("Task 1:")

  def pair: Iterator[(Int, Int)] = for {
    a <- Iterator.from(1)
    b <- 1 until a + 1 if a % b == 0
  } yield (a, b)

  println("Using next:")
  var x = pair.buffered
  for (i <- List.range(0, 20)) println(x.next)

  println("Method take:")
  val y = pair.take(20)
  for (i <- y) println(i)

  println("Task 2:")
  val g = for {
    a <- Yes("test")
  } yield a
  println(g)

}

