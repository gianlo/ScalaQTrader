package net.gianlorenzo.scalaqtrader.model

import java.time.LocalDate

import net.gianlorenzo.scalaqtrader.io.ExtractFromRow

final case class GSPC(date: LocalDate, adjClose:BigDecimal)

object GSPC{
  implicit val extractor = new ExtractFromRow[GSPC] {
    override def extract(row: Array[String]): GSPC = GSPC(LocalDate.parse(row(0)), BigDecimal(row(5)))
  }
}
