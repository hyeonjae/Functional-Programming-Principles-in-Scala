package Lecture_2_1_Tail_Recursion

import annotation.tailrec

object scala extends App {

  def abs(x: Double) = if (x<0) -x else x

  def sqrt(x: Double) = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess*guess - x) / x < 0.001
    def improve(guess: Double, x: Double) =
      (guess + x/guess) / 2

    sqrtIter(1.0, x)
  }

  @tailrec
  def gcd(x: Int, y: Int): Int =
    if (y == 0) x else gcd(y, x % y)

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if (n < 2) acc else loop(n - 1, acc * n)
    loop(n, 1)
  }

  println("sqrt(): " + sqrt(2))
  println("sqrt(): " + sqrt(3))
  println("sqrt(): " + sqrt(4))

  println("gcd(): " + gcd(4, 3))
  println("gcd(): " + gcd(12, 3))
  println("gcd(): " + gcd(12, 8))

  println("factorial(): " + factorial(4))
  println("factorial(): " + factorial(10))
  println("factorial(): " + factorial(100))
}
