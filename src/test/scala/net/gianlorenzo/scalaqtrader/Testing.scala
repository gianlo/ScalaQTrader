package net.gianlorenzo.scalaqtrader

import java.time.LocalDate

import net.gianlorenzo.scalaqtrader.io.CsvReader
import net.gianlorenzo.scalaqtrader.model.{GSPC, TBill}
import net.gianlorenzo.scalaqtrader.trader.LoadData

object Testing {
  def main(args: Array[String]): Unit = {
    val data = LoadData(
      CsvReader.read[TBill](getClass.getResourceAsStream("/tbill.csv")),
      CsvReader.read[GSPC](getClass.getResourceAsStream("/^GSPC.csv")))
    val date = LocalDate.parse("2002-05-06")
    println(data.tbill(date))
    println(data.stocks(date))
  }
}
