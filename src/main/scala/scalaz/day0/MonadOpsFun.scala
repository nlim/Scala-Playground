object MonadOpsFun {

  trait Monad[M[_]] {
    def point[A](a: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => point(f(a)))
  }

  case class MonadOps[M[_], A](ma: M[A], m: Monad[M]) {
    def flatMap[B](f: A => M[B]): M[B] = m.flatMap(ma)(f)
    def map[B](f: A => B): M[B] = m.map(ma)(f)
  }

  implicit def toMonadOps[M[_]: Monad, A](ma: M[A]) =
    MonadOps[M, A](ma, implicitly[Monad[M]])

  trait Either[+A, +B]
  case class Left[A](a: A) extends Either[A, Nothing]
  case class Right[B](b: B) extends Either[Nothing, B]

  implicit def eitherMonad[L] = new Monad[({type l[a] = Either[L, a]})#l] { 
    def flatMap[A, B](fa: Either[L, A])(f: A => Either[L, B]) = fa match {
      case Left(a)  => Left(a)
      case Right(b) => f(b)
    }
    def point[A](a: A) = Right(a)
  }

  val r1: Either[String, Int] = Right(10)
  val r2: Either[String, Int] = Right(12)

  val result =
    for {
      a <- toMonadOps(r1)
      b <- toMonadOps(r2)
    } yield a + b
}
