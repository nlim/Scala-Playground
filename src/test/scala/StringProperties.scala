import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object StringSpecification extends Properties("String") {

  property("reverse . reverse") = forAll { l: List[String] => l.reverse.reverse == l }
  property("uncons . cons")     = forAll { (s: String, l: List[String]) => s == (s :: l).head }
}
