import scala.language.higherKinds;

object PureIO {

  trait Monad[M[_]] {
    def unit[A](a: => A): M[A]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
  }

  type Id[A] = A

  implicit object MonadId extends Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    def flatMap[A,B](ia: Id[A])(f: A => Id[B]): Id[B] = f(ia)
  }



  sealed abstract class IO[F[_], A] extends {
    def flatMap[B](f: A => IO[F, B]): IO[F, B] = {
      this match {
        case Pure(a) => f(a)
        case Request(fi, k) => Request(fi, k andThen (_ flatMap f))
        case FlatMap(sub, k) => FlatMap(sub, k andThen (_ flatMap f))
      }
    }

    // Running IO is essentially applying the Natural transformation
    // from the context in which we run the request F, to another context
    // G (which could be Id) from which we can attain the value
    def runIO[G[_]](f: F ~> G)(implicit G: Monad[G]): G[A] = {
      this match {
        case Pure(a) => G.unit(a)
        case Request(fi, c) => G.flatMap(f(fi))(c andThen (_.runIO(f)))
      }
    }
  }

  case class Pure[F[_], A](a: A) extends IO[F, A]

  case class Request[F[_], I, A](fi: F[I], c: I => IO[F,A]) extends IO[F, A]

  case class FlatMap[F[_], A, B](sub: F[A], k: A => IO[F, B]) extends IO[F, B]

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  sealed trait Console[A]
  case object GetLine extends Console[String]
  case class PutLine(s: String) extends Console[Unit]

  /*
  type ConsoleIO[A] = IO[Console, A]

  def getLine: ConsoleIO[String] = Request(GetLine, s => Pure(s))
  def putLine: ConsoleIO[Unit]   = Request(PutLine, _ => Pure(()))


  val ask: ConsoleIO[Unit] = for {
    _    <- putLine("What is your name?")
    name <- getLine
    _    <- putLine("Hello, " ++ name)
  } yield ()

  type AnyIO[A] = IO[Function0, A]
  */

  object SideEffect extends (Function0 ~> Id) {
    def apply[A](f: Function0[A]): A = f()
  }


 object Program {
    def unsafePerformIO[A](io: IO[Function0, A]) = io.runIO(SideEffect)
  }
}


