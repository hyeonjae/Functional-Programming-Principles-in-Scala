package Lecture_2_2_Higher_Order_Functions

object scala extends App {
  def id(x: Int): Int = x
  def square(x: Int): Int = x*x
  def cube(x: Int): Int = x*x*x
  def fact(x: Int): Int = if (x<2) 1 else x*fact(x-1)

  def Sigma(f: Int => Int, a: Int, b: Int): Int = {
    if (a>b)
      0
    else
      f(a) + Sigma(f, a+1, b)
  }

  def sumInts(a: Int, b: Int): Int = Sigma(id, a, b)
  def sumSquares(a: Int, b: Int): Int = Sigma(square, a, b)
  def sumCubes(a: Int, b: Int): Int = Sigma(cube, a, b)
  def sumFactorials(a: Int, b: Int): Int = Sigma(fact, a, b)

  println("sumInts(3, 5): " + sumInts(3, 5))
  println("sumSquares(3, 5): " + sumSquares(3, 5))
  println("sumCubes(3, 5): " + sumCubes(3, 5))
  println("sumFactorials(3, 5): " + sumFactorials(3, 5))

  // Anonymous Function
  println("Anonymous Function(3, 5): " + Sigma((x: Int) => x*x*x, 3, 5))

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, acc+f(a))
    }
    loop(a, 0)
  }

  // Tail-recursive Version
  println("Tail-recursive(3, 5): " + sum(x => x*x*x, 3, 5))
}
