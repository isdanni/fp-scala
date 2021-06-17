package fpinscala.datastructures

sealed trait List[+A]	
case object Nil extends List[Nothing]	
case class Cons[+A](head: A, tail: List[A]) extends List[A]	

object List {	
  def sum(ints: List[Int]): Int = ints match {	
    case Nil => 0	
    case Cons(x,xs) => x + sum(xs)	
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =	
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL	
    val nextRNG = SimpleRNG(newSeed)	
    val n = (newSeed >>> 16).toInt	
    (n, nextRNG)	
  }
}
