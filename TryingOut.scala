import scala.util.Try

object TryingOut extends App {

  override def main(args: Array[String]) {
    List(0, 6, 1, 2, 3, 4, 5).foreach { i =>
     println(i + " as input results in: " + calculateResult(i))
    }
  }


  def calculateResult(i: Int): Try[Int] = {
    for {
      a <- foo(i)
      b <- bar(a)
      c <- baz(b)
    } yield c
  }



  def foo(i: Int): Try[Int] = {
    Try {
      18 / i
    }
  }

  def bar(i: Int): Try[Int] = {
    Try {
      i - 3
    }
  }

  def baz(i: Int): Try[Int] = {
    Try {
      5 / i
    }
  }



}
