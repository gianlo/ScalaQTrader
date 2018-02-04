package net.gianlorenzo.scalaqtrader.model

class Q[S, A] private(val m : Map[S, Map[A, Double]]) {

  def update(s: S, a: A, v: Double) =
    new Q(
      m.updated(s,
        m.get(s).fold(Map(a -> v))(_.updated(a, v))
      ))
}

object Q{
  def empty[S, A] = new Q[S, A](Map.empty[S, Map[A, Double]])
}
