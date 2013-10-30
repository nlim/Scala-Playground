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
        case Request(fi, c) => Request(fi, c andThen (_.flatMap(f)))
        // case FlatMap(sub, k) => FlatMap(sub, (x: Any) => k(x).flatMap(f))
        //case x => FlatMap(x, f)
      }
    }

    def map[B](f: A => B): IO[F, B] = flatMap((a: A) => Pure(f(a)))

    // Running IO is essentially applying the Natural transformation
    // from the context in which we run the request F, to another context
    // G (which could be Id) from which we can attain the value
     final def runIO[G[_]](f: F ~> G)(implicit G: Monad[G]): G[A] = {
       this match {
         case Pure(a) => G.unit(a)
         case Request(fi, c) => G.flatMap(f(fi))(c andThen (_.runIO(f)))
       }
     }
  }

  case class Pure[F[_], A](a: A) extends IO[F, A]
  case class Request[F[_], I, A](fi: F[I], c: I => IO[F,A]) extends IO[F, A]
  // case class FlatMap[F[_], A, B](sub: IO[F, A], k: A => IO[F, B]) extends IO[F, B]

  trait ~>[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  sealed trait Console[A]
  case object GetLine extends Console[String]
  case class PutLine(s: String) extends Console[Unit]

  
  type ConsoleIO[A] = IO[Console, A]

  val getLine: ConsoleIO[String] = Request(GetLine, (s: String) => Pure(s))
  def putLine(s: String): ConsoleIO[Unit] = Request(PutLine(s), (_: Unit) => Pure(()))


  val ask: ConsoleIO[Unit] = for {
    _    <- putLine("What is your name?")
    name <- getLine
    _    <- putLine("Hello, " ++ name)
  } yield ()


  type AnyIO[A] = IO[Function0, A]

  object SideEffect extends (Function0 ~> Id) {
    def apply[A](f: Function0[A]): A = f()
  }

  object ConsoleEffect extends (Console ~> Id) {
    def apply[A](c: Console[A]): A = {
      c match {
        case GetLine => readLine
        case PutLine(s) => println(s)
      }
    }
  }

  case class InOut(in: List[String], out: List[String])
  case class State[A](runState: InOut => (A, InOut))

  implicit object StateMonad extends Monad[State] {
    def unit[A](a: => A): State[A] = State(s => (a, s))
    def flatMap[A, B](sa: State[A])(f: A => State[B]): State[B] = 
      State(s => {
        val (a, s2) = sa.runState(s)
        val (b, s3) = f(a).runState(s2)
        (b, s3)
      })
  }


  object PureConsole extends (Console ~> State) {
    def apply[A](c: Console[A]): State[A] = {
      State(s => (c, s) match {
        case (GetLine, InOut(i, o)) => (i.head, InOut(i.tail, o))
        case (PutLine(s), InOut(i, o)) => ((), InOut(i, s :: o)) 
      })
    }
  }


  def unsafePerformIO[A](io: IO[Function0, A]) = io.runIO(SideEffect)
}


