package net.gianlorenzo.scalaqtrader.model

final case class Fraction private(f: Double)

object Fraction {

  implicit def toDouble(ff: Fraction): Double = ff.f

  def create(f: Double): Fraction =
    {
      assert(f>= 0.0 && f <= 1.0)
      Fraction(f)
    }
}
