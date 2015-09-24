object scala extends App {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  println("x: " + x)
  println("y: " + y)
  println("z: " + z)
  println("x+y: " + x.add(y))

  println("-x: " + x.neg)
  println("-y: " + y.neg)
  println("-z: " + z.neg)
  println("x-y-z: " + x.sub(y).sub(z))
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  def neg: Rational = new Rational(-x, y)

  def add(that: Rational) =
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)

  def sub(that: Rational) =
    this.add(that.neg)

  override def toString = numer + "/" + denom
}