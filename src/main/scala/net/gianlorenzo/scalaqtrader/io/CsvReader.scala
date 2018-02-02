package net.gianlorenzo.scalaqtrader.io

import java.io.InputStream

import scala.io.Source

trait ExtractFromRow[T]{
  def extract(row: Array[String]): T
}

object CsvReader {

  def read[T](fileStream: InputStream, skipLines: Int=1)(implicit ev: ExtractFromRow[T]): Stream[T] = Source
    .fromInputStream(fileStream)
    .getLines()
    .drop(skipLines)
    .map(_.split(",", -1))
    .toStream
    .map(ev.extract)
}

