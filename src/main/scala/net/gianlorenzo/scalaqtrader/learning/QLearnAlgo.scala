package net.gianlorenzo.scalaqtrader.learning

import net.gianlorenzo.scalaqtrader.model.{Fraction, Q}


trait Randomizer[+T] {
  def sample(): T
}

trait Enumerate[T] {
  def getAll(): Set[T]
}

trait ChooseDefault[+T] {
  def default: T
}

final case class Experience[+S](state: S, reward: Double)

trait Environment[+S, -A] {
  def evolve(action: A): (Environment[S, A], Experience[S])
}

trait HasTerminal[+S] {
  def terminal: S
}

class QLearnAlgo[S, A](learningRate: Fraction, discountFactor: Fraction, randomActionFraction: Fraction) {

  import Fraction._

  private val actionChooserRandomiser = new scala.util.Random()

  private def chooseNextActionDeterministically: Boolean = actionChooserRandomiser.nextDouble() > 1.0 - randomActionFraction

  private def initQ(implicit es: Enumerate[S], ea: Enumerate[A]): Q[S, A] = (for {
    s <- es.getAll()
    a <- ea.getAll()
  } yield (s, a)).foldLeft(Q.empty[S, A]) { case (q, (s, a)) => q.update(s, a, 0.0) }


  private def epsGreedy(currState: S, q: Q[S, A])(implicit ar: Randomizer[A]) = if (chooseNextActionDeterministically) {
    q.greedyAction(currState)
  } else {
    ar.sample()
  }

  def train(environment: Environment[S, A])(implicit ar: Randomizer[A], es: Enumerate[S], da: Enumerate[A], sf: HasTerminal[S]): Q[S, A] = {

    def go(currState: S, env: Environment[S, A], qcurr: Q[S, A])(implicit ar: Randomizer[A], sf: HasTerminal[S]): Q[S, A] = {

      val action = epsGreedy(currState, qcurr)

      val (newEnv, Experience(nextState, reward)) = env.evolve(action)

      val qUpdate = for {
        value <- qcurr.m(nextState).get(action)
        learned = reward + (discountFactor * value)
        prevValue <- qcurr.m(currState).get(action)
      } yield prevValue + learningRate * (learned - prevValue)

      val newQ =  qcurr.update(currState, action, qUpdate.getOrElse(0.0))

      if (nextState == sf.terminal) newQ else go(nextState, newEnv, newQ)
    }


    (1 to 1000).foldLeft(initQ){ case (q, _) =>
      val (newEnv, Experience(initState, _)) = environment.evolve(ar.sample())
      go(initState, newEnv, q)
    }
  }

}



final case class SimpleEnv private(internalState: Int) extends Environment[Int, Int] {

  private def newState(action: Int): Int = {
    val nextState = (action + internalState) % 10
    if (nextState <= 0)
      nextState + 10
    else
      nextState
  }

  override def evolve(action: Int): (Environment[Int, Int], Experience[Int]) = (SimpleEnv(newState(action)), Experience(newState(action), if (newState(action) == 10) 0 else -1))
}

object SimpleEnv{
  def create: SimpleEnv = SimpleEnv(1)
}

object QLearnAlgo {
  def main(args: Array[String]): Unit = {
    val qlearner = new QLearnAlgo[Int, Int](Fraction(0.8), Fraction(0.2), Fraction(0.001))

    val intR = new Randomizer[Int] {
      private val rnd = new scala.util.Random()

      override def sample(): Int = rnd.nextInt(5) - 2
    }

    val intD = new ChooseDefault[Int] {
      override def default: Int = 0
    }

    val intE = new Enumerate[Int] {
      override def getAll(): Set[Int] = 1 to 10 toSet
    }

    val intHF = new HasTerminal[Int] {
      override def terminal: Int = 10
    }

    val env = SimpleEnv.create

    val q = qlearner.train(env)(ar = intR,
      es = intE, da = new Enumerate[Int] {
        override def getAll(): Set[Int] = -2 to 2 toSet
      }, sf = intHF)

    println((1 to 20).scanLeft(SimpleEnv.create.asInstanceOf[Environment[Int, Int]]){ case (s, _) => s.evolve(1)._1})
    val policy = q.m.mapValues(av => av.maxBy(_._2)._1).toList.sortBy(_._1)
    println("Q")
    println(q.m.mapValues(_.toList.sortBy(_._1)).toList.sortBy(_._1))
    println("Policy")
    println(policy)
  }
}
