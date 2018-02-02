package net.gianlorenzo.scalaqtrader.model

import java.time.LocalDate

import net.gianlorenzo.scalaqtrader.io.ExtractFromRow

final case class TBill(date: LocalDate, tbill_rate: BigDecimal)


object TBill{
  implicit val extractor = new ExtractFromRow[TBill] {
    override def extract(row: Array[String]): TBill = TBill(LocalDate.parse(row(0)), BigDecimal(row(1)))
  }
}


