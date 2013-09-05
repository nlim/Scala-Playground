object Summing {

  trait Monoid[A] {
    def mappend(a1: A, a2: A): A
    def mzero: A
  }

  object Monoid {
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    implicit val StringMonoid: Monoid[String] = {
      new Monoid[String] {
        def mappend(a: String, b: String): String = a + b
        def mzero: String = ""
      }
    }

    val MultiMonoid: Monoid[Int] = {
      new Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a * b
        def mzero: Int = 1
      }
    }
  }

  trait MonoidOps[A] {
    val F: Monoid[A]
    val value: A
    def |+|(a2: A) = F.mappend(value, a2)
  }

  object MonoidOps {
    implicit def intToMonoidOps(i: Int) = {
      new MonoidOps[Int] {
        val F = Monoid.IntMonoid
        val value = i
      }
    }
  }

  trait FoldLeft[F[_]] {
    def evaluate[A, B](data: F[A], start: B)(f: (B,A) => B): B
  }

  object FoldLeft {
    implicit val listFoldLeft = new FoldLeft[List] {
      def evaluate[A, B](list: List[A], start: B)(f: (B, A) => B) = list.foldLeft(start)(f)
    }
  }

  def sum[F[_]: FoldLeft, M: Monoid](xs: F[M]): M = {
    val m = implicitly[Monoid[M]]
    val fl = implicitly[FoldLeft[F]]
    fl.evaluate(xs, m.mzero)(m.mappend)
  }
}

