package net.gianlorenzo.scalaqtrader.io

import java.time.LocalDate

import net.gianlorenzo.scalaqtrader.model.{GSPC, TBill}
import org.scalatest.{FunSuite, MustMatchers}

class CsvReaderTest extends FunSuite with MustMatchers {

  test("testRead for TBill") {
    val lines = CsvReader.read[TBill](getClass.getResourceAsStream("/tbill.csv"))
    lines.head mustBe TBill(LocalDate.of(2002, 5, 2), BigDecimal("1.74"))
    lines.toList.size mustBe 3922
  }

  test("testRead for GSPC") {
    val lines = CsvReader.read[GSPC](getClass.getResourceAsStream("/^GSPC.csv"))
    lines.head mustBe GSPC(LocalDate.of(1950, 1, 9), BigDecimal("16.65"))
    lines.toList.size mustBe 3530
  }

}
