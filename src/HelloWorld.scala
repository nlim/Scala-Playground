import scala.util.Random;
import scala.collection.mutable.HashMap

object HelloWorld {
	val rand = new Random()
	  
	abstract class Expr
	case class Number(value:Int) extends Expr
	case class Sum(left: Expr, right: Expr) extends Expr
	case class Prod(left: Expr, right: Expr) extends Expr
	case class Div(left: Expr, right: Expr) extends Expr
	
	
	// Pattern Matching Over Classes of Expr
	def eval(e: Expr): Int = e match { 
		case Number(n)  => n
		case Sum (l, r) => eval(l) + eval(r)
		case Prod(l, r) => eval(l) * eval(r)
		case Div (l, r) => eval(l) / eval(r)
	}

	
	
	abstract class Stack[A] {
		def push(x: A): Stack[A] = new NonEmptyStack[A](x, this)
		def isEmpty: Boolean
		def top: A
		def pop: Stack[A]
	}
	class EmptyStack[A] extends Stack[A] {
		def isEmpty = true
		def top = error("EmptyStack.top") 
		def pop = error("EmptyStack.pop")
	}
	class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
		def isEmpty = false 
		def top = elem
		def pop = rest
	}
	
	def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)
	
	def factorial_tail(n: Int): Int = {
	  def fact_helper(input: Int, accum: Int): Int = if (input <= 1) accum else fact_helper(input-1, input*accum)
	  fact_helper(n, 1)
	}
	
	//Calculate the nth fibonacci number
	def fib(n: Int): Int = if (n <= 2) 1 else fib(n-1) + fib(n-2)
	
	
	//Curried Function to add a some constant to the input
	def addnum(n: Int)(x: Int) = n + x
  
	
	
	
	def quicksort(list: List[Int]): List[Int] = {
	  val size = list.length
	  if (list.length <= 1) {
		  list
	  } else {
		  val pivot = list(size/2)
		  quicksort(list.filter(pivot >)) :::
				    list.filter(pivot ==) :::
		  quicksort(list.filter(pivot <))
	  } 
	}
	
	
	
	def mergesort(list: List[Int]): List[Int] = {
	  def merge(left: List[Int], right: List[Int]): List[Int] = {
	    if (left.length < 1) {
	    	right
	    } else if (right.length < 1) {
	    	left
	    } else {
	    	if   (left.head < right.head) left.head :: merge(left.tail, right)
	    	else right.head :: merge(left, right.tail)
	    }	     
	  }
	  val cut_one = list.slice(0, list.length/2)
	  val cut_two = list.slice(list.length/2, list.length)
	  if (cut_one.length < 1) {
		  cut_two
	  } else if (cut_two.length < 1) {
		  cut_one
	  } else {
		  merge(mergesort(cut_one), mergesort(cut_two))
	  }
	}
	
	def randIntList(size: Int, range: Int): List[Int] = {
	  for (i <- List.range(1, size)) yield rand.nextInt(range)
	}
	
	
	
	def main (args: Array[String]) {
		println("Hello world, starting to program in Scala!")
		var expression = Prod(Sum(Number(4), Sum(Number(5), Number(2))), Number(10))
		println("The number is: " +  eval(expression))
		var list = List.range(1, 10)
		println(list)
		println(list map fib)
		println(list map factorial)
		println(list map factorial_tail)
		println(list map addnum(4))
		println(list map addnum(12))
		list = randIntList(10, 100)
		println(list)
		println(quicksort(list))
		println(mergesort(list))
		println(list.reduceLeft((a, b) => a+b))
		println(list.reduceLeft((a, b) => a*b))
		
	}
}