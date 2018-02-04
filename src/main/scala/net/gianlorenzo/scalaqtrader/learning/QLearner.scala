package net.gianlorenzo.scalaqtrader.learning

import net.gianlorenzo.scalaqtrader.model.{Fraction, Q}

trait DataToState[-D, +S]{
  def toState(data: D): S
}

trait WithBase[+T]{
  def base: T
}

final case class Factor[A, S](action: A, reward: Double, state: S)

class QLearner[S, A](alpha: Fraction, gamma: Fraction, nIterations: Int)(implicit sb: WithBase[S], ab: WithBase[A]) {

  type F = Factor[A, S]

  require(nIterations > 0)

  def train[I, D](trainingData: Map[I, D], hasConverged: Iterable[A] => Boolean)(implicit d2s: DataToState[D, S]): Q[S, A] = {

    def go(factors: Map[I, F], iteration: Int, qacc: Q[S, A]): Q[S,A] = if (iteration > nIterations) {
      qacc
    } else {
      // compute updates
      println(s"Iteration ${iteration}/${nIterations}")
      // check convergence
      if (hasConverged(factors.values.map(_.action))){
        qacc
      } else {
        go(factors, iteration + 1, qacc)
      }
    }

    go(trainingData.mapValues(_ => Factor(ab.base, 0.0, sb.base)), 1, Q.empty[S, A])
  }
}

object QLearner {

  sealed trait Action
  case object Buy extends Action
  case object Sell extends Action
  case object DoNothing extends Action

  object Action{
    implicit val stateBase = new WithBase[Position] {
      override def base: Position = PNeutral
    }
  }

  sealed trait Position
  case object PLong extends Position
  case object PShort extends Position
  case object PNeutral extends Position

  object Position {
    implicit val actionBase = new WithBase[Action] {
      override def base: Action = DoNothing
    }
  }

  def main(args: Array[String]): Unit = {
    import Action._
    import Position._

    val q = new QLearner[Position, Action](Fraction.create(0.8), Fraction.create(0.2), 10)
    implicit val d2s = new DataToState[Int, Position] {
      override def toState(data: Int): Position = data match {
        case p if p > 0   => PLong
        case n if n < 0   => PShort
        case z if z == 0  => PNeutral
      }
    }
    q.train(Map.empty[Int, Int], _.nonEmpty)
  }
}