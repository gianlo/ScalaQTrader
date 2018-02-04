package net.gianlorenzo.scalaqtrader.trader

import java.time.LocalDate

import net.gianlorenzo.scalaqtrader.model.{GSPC, TBill}

case class LoadData(tbillsData: Stream[TBill], gspcData: Stream[GSPC]) {

  val tbill: Map[LocalDate, Double] = tbillsData
    .groupBy(_.date)
    .mapValues(vs => scala.math.pow(vs.head.tbill_rate.toDouble/100 + 1, 1.0/52.0) - 1)

  val stocks: Map[LocalDate, Double] = gspcData
    .groupBy(_.date)
    .mapValues( vs => vs.head.adjClose.toDouble)

}
