import org.scalacheck.Prop._

object StringProperties {

  trait Show[T] {
    def show(t: T): String
  }

  object StringShow extends Show[String] {
    def show(s: String) = s
  }

  implicit def showList[T](implicit ts: Show[T]) = new Show[List[T]] {
    def show(lt: List[T]) = lt.map(ts.show(_)).mkString("[", ",", "]")
  }

  def show[W](w: W)(implicit sw: Show[W]) = sw.show(w)

  // val x = show(List("Foo", "bar"))

  val prop1 = forAll { l: List[String] => l.reverse.reverse == l }
  val prop2 = forAll { l: List[String] => l.head :: l.tail == l }
}
