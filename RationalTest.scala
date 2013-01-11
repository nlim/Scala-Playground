package test

import numbers.Rational

object RationalTest extends App {
  println("Starting program")
  var num = new Rational(3, 5)
  println("My Cool Rational Number: " + num)
  num = new Rational(5)
  println("Another Cool Rational Number: " + num)
}

