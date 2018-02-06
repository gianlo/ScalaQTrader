package net.gianlorenzo.scalaqtrader.learning

import net.gianlorenzo.scalaqtrader.model.{Fraction, Q}


trait Randomizer[+T]{
  def sample(): T
}

trait Enumerate[+T]{
  def getAll(): Seq[T]
}

trait ChooseDefault[+T]{
  def default: T
}

class QLearnAlgo[S, A](learningRate: Fraction, discountFactor: Fraction, randomActionFraction: Fraction) {

  import Fraction._

  private val actionChooserRandomiser = new scala.util.Random()

  private def chooseNextActionDeterministically: Boolean = actionChooserRandomiser.nextDouble() > 1.0 - randomActionFraction

  def initQ(implicit es: Enumerate[S], da: ChooseDefault[A]): Q[S, A] = es.getAll().foldLeft(Q.empty[S, A]){case (q, s) => q.update(s, da.default, 0.0)}

  def train(trainingData: Seq[(S, Double)])(implicit ar: Randomizer[A], es: Enumerate[S], da: ChooseDefault[A]): Q[S, A] = {

    def go(experience: (S, Double), data: Seq[(S, Double)], qcurr: Q[S, A])(implicit ar: Randomizer[A], es: Enumerate[S], da: ChooseDefault[A]): Q[S, A] = data match {
      case (nextState, nextReward) +: tail => {
        val (state, reward) = experience
        val action = if (chooseNextActionDeterministically) {
          qcurr.m(state).maxBy(_._2)._1
        } else {
          ar.sample()
        }
        val qUpdate = for {
          value <-  qcurr.m(state).get(action)
          learned = reward + (discountFactor * value)
        } yield (1.0 - learningRate) * value + learningRate * learned
        go((nextState, nextReward), tail, qcurr.update(state, action, qUpdate.getOrElse(0.0)))
      }
      case _ => qcurr
    }

    go(trainingData.head, trainingData.tail, initQ)
  }
}


object QLearnAlgo {
  def main(args: Array[String]): Unit = {
    val qlearner = new QLearnAlgo[Int, Int](Fraction(0.8), Fraction(0.2), Fraction(0.01))

    implicit val intR = new Randomizer[Int] {
      private val rnd = new scala.util.Random()
      override def sample(): Int = 2-rnd.nextInt(5)
    }

    implicit val intD = new ChooseDefault[Int] {
      override def default: Int = 0
    }

    implicit val intE = new Enumerate[Int] {
      override def getAll(): Seq[Int] = (1 to 10)
    }

    val trainingSet = (1 to 100).map(n => ((n % 10) + 1, n.toDouble + 0.03))
    val q = qlearner.train(trainingSet)

    val policy = q.m.mapValues(av => av.maxBy(_._2)._1)
    println("Q")
    println(q.m)
    println("Policy")
    println(policy)
  }
}
