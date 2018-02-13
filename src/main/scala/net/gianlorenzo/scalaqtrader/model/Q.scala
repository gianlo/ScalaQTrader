package net.gianlorenzo.scalaqtrader.model

class Q[S, A] private(val m : Map[S, Map[A, Double]]) {

  def apply(s: S, a: A): Option[Double] = for {
    av <- m.get(s)
    v  <- av.get(a)
  } yield v

  def update(s: S, a: A, v: Double) =
    new Q(
      m.updated(s,
        m.get(s).fold(Map(a -> v))(_.updated(a, v))
      ))

  def greedyAction(s: S): A = m(s).maxBy(_._2)._1
}

object Q{
  def empty[S, A] = new Q[S, A](Map.empty[S, Map[A, Double]])
}
