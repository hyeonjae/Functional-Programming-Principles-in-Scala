package Lecture_2_4_Finding_Fixed_Points

import math.abs

object scala extends App {
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x-y) / x) / x < tolerance
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  println("fixedPoint: " + fixedPoint(x=>1+x/2)(1))

  def sqrt(x: Double) = fixedPoint(y => (y+x/y)/2)(1)
  println("sqrt(2): " + sqrt(2))

  def averageDamp(f: Double => Double)(x: Double): Double = (x+f(x))/2
  def sqrt2(x: Double) = fixedPoint(averageDamp(y=>x/y))(1)
  println("sqrt2(2): " + sqrt2(2))
}
