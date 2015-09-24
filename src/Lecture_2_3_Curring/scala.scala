package Lecture_2_3_Curring

object scala extends App {

  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }
  println("sum (x=>x*x) (3, 5): " + sum(x=>x*x)(3, 5))


  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum2(f)(a + 1, b)
  }
  println("sum2 (x=>x*x) (3, 5): " + sum2(x=>x*x)(3, 5))

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def factorial(n: Int): Int = {
    product(x=>x)(1, n)
  }
  println("factorial(5): " + factorial(5))

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, initial: Int)(a: Int, b: Int): Int = {
    if (a>b) initial
    else combine(f(a), mapReduce(f, combine, initial)(a+1, b))
  }
  println("mapReduce(x=>x, (a,b)=>(a+b), 0)(3,5): " + mapReduce(x=>x, (a,b)=>(a+b), 0)(3,5))
  println("mapReduce(x=>x, (a,b)=>(a*b), 1)(3,5): " + mapReduce(x=>x, (a,b)=>(a*b), 1)(3,5))
}
