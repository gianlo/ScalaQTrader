package net.gianlorenzo.scalaqtrader.learning

import net.gianlorenzo.scalaqtrader.model.{Fraction, Q}

class QLearnAlgo[S, A](learningRate: Fraction, discountFactor: Fraction) {

  import Fraction._

  def initQ: Q[S,A] = Q.empty[S, A]

  def train[I](trainingData: Stream[(I, S, Double)]): Q[S, A] = {

    implicit def dataToExp(d: (I, S, Double)): (S, Double) = (d._2, d._3)

    def go(experience: (S, Double), data: Stream[(I, S, Double)], qcurr: Q[S,A]): Q[S, A] = data match {
      case (_, nextState, nextReward) #:: tail => {
        val (state, reward) = experience
        val action = qcurr.m(state).maxBy(_._2)._1
        val learned = reward + (discountFactor * qcurr.m(state)(action))
        val qupdate = (1.0 - learningRate) * qcurr.m(state)(action) + learningRate * learned
        go((nextState, nextReward), tail, qcurr.update(state, action, qupdate))
      }
      case _ => qcurr
    }

    go(trainingData.head, trainingData.tail, initQ)
  }

}
