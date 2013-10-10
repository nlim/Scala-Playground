import scala.language.higherKinds

object HigherRank {

  val singletonList = new (Id ~> List) {
    def apply[A](a: A): List[A] = List(a)
  }

  // Can't resolve the type here because
  // having the universal quantification of our polymorphic f
  // be in the scope of the whole function, it must be bound to
  // a specific A for a given application of the apply function
  //
  // found   : b.type (with underlying type B)
  // required: A
  //  def apply[A,B](f: A => List[A], b: B, s: String): (List[B], List[String]) = (f(b), f(s))
  //                                                                                 ^
  // found   : s.type (with underlying type String)
  // required: A
  //  def apply[A,B](f: A => List[A], b: B, s: String): (List[B], List[String]) = (f(b), f(s))
  //                                                                                      ^
  //
  //

  def applyPoly[B](f: Id ~> List, b: B, s: String): (List[B], List[String]) = (f(b), f(s))

  def applyPoly2[B, C](f: Id ~> List, b: B, c: C): (List[B], List[C]) = (f(b), f(c))

  def applyPoly3[B, C](f: Nat2[Id, Id, List], b1: B, b2: B, c1: C, c2: C): (List[B], List[C])  =
    (f(b1)(b2), f(c1)(c2))

  val runPoly = applyPoly2(singletonList, 'c', 5)


  val fuser = new Nat2[Id, Id, List] {
    def apply[A](a: A)(a2: A) = List(a, a2)
  }

  type Id[A] = A

  trait ~>[F[_],G[_]] {
      def apply[A](a: F[A]): G[A]
  }

  trait Nat2[F[_], G[_], H[_]] {
    def apply[A](f: F[A])(g: G[A]): H[A]
  }
}
