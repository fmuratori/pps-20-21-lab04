package u04lab.code

trait Complex2 {
  def re: Double
  def im: Double
  def +(c: Complex2): Complex2 // should implement the sum of two complex numbers..
  def *(c: Complex2): Complex2 // should implement the product of two complex numbers
}
case class ComplexImpl(re: Double, im:Double) extends Complex2 {
  override def +(c: Complex2): Complex2 = new ComplexImpl(this.re + c.re, this.im + c.im)

  override def *(c: Complex2): Complex2 = new ComplexImpl(this.re * c.re, this.im * c.im)
}

object Complex2 {
  def apply(re:Double, im:Double):Complex2 = ComplexImpl(re, im)
}

object TryComplex2 extends App {
  val a = Array(Complex2(10,20), Complex2(1,1), Complex2(7,0))
  val c = a(0) + a(1) + a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
}

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */