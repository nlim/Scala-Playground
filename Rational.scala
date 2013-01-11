package numbers

class Rational(n: Int, d: Int) {
  
  def this(n: Int) = this(n, 1)
  
  override def toString = n + "/" + d
}



