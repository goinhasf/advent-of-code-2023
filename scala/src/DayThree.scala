package net.fearlessmind

import scala.collection.mutable
import scala.collection.immutable.Range.Exclusive

object DayThree:
  case class Symbol(row: Int, col: Int, symbol: Char)
  object P1:
    def findSymbols(lines: Seq[String]): Seq[Symbol] =
      lines.indices.flatMap { index =>
        val line = lines(index)
        val nonDigitsRegex = "\\D".r
        line.zipWithIndex
          .filter((c, _) => c != '.' && nonDigitsRegex.matches(c.toString()))
          .map((c, col) => Symbol(index, col, c))

      }

    def getNumberRanges(lines: Seq[String]): Array[Seq[Range]] = lines
      .map("\\d+".r.findAllMatchIn)
      .map(matches => matches.map(m => Range(m.start, m.end)).toSeq)
      .toArray

    def rangeToNumber(line: String, range: Range) =
      line.substring(range.start, range.end).toInt

    def findNumbersOverlaping(
        symbolCoordinates: (Int, Int),
        numbersInRow: mutable.Seq[(Range, Int)]
    ) =
      val (row, col) = symbolCoordinates
      val range = Range(row, col).inclusive
      numbersInRow
        .filterNot((r, _) => range.intersect(r).isEmpty)

    def getAdjacentNumbers(lines: Array[String]) =
      val symbols = findSymbols(lines)
      val maxRowIndex = lines.length - 1
      val maxColIndex = lines(0).size - 1
      val numberRanges = getNumberRanges(lines)
      val numbersAvailable = numberRanges.zipWithIndex
        .map((ranges, index) =>
          ranges.map(r => (r, rangeToNumber(lines(index), r)))
        )
        .map(mutable.ArrayBuffer.from)
      val numbers = mutable.ListBuffer[(Range, Int)]()

      symbols.foreach { coordinates =>
        val Symbol(row, col, _) = coordinates
        // Top left
        if (row == 0 && col == 0)
          val row1 = numbersAvailable(0)
          val row2 = numbersAvailable(1)
          val found1 = findNumbersOverlaping((1, 1), row1)
          val found2 = findNumbersOverlaping((0, 1), row2)
          numbersAvailable(0) --= found1
          numbersAvailable(1) --= found2
          numbers ++= (found1 ++ found2)

        // Top right
        if (row == 0 && col == maxColIndex)
          val row1 = numbersAvailable(0)
          val row2 = numbersAvailable(1)
          val found1 =
            findNumbersOverlaping((maxColIndex - 1, maxColIndex - 1), row1)
          val found2 =
            findNumbersOverlaping((maxColIndex - 1, maxColIndex), row2)
          numbersAvailable(0) --= found1
          numbersAvailable(1) --= found2
          numbers ++= (found1 ++ found2)

        // Bottom left
        else if (row == maxRowIndex && col == 0)
          val row1 = numbersAvailable(maxRowIndex)
          val row2 = numbersAvailable(maxRowIndex - 1)
          val found1 =
            findNumbersOverlaping((1, 1), row1)
          val found2 =
            findNumbersOverlaping((0, 1), row2)
          numbersAvailable(maxRowIndex) --= found1
          numbersAvailable(maxRowIndex - 1) --= found2
          numbers ++= (found1 ++ found2)

        // Bottom right
        else if (row == maxRowIndex && col == maxColIndex)
          val row1 = numbersAvailable(maxRowIndex)
          val row2 = numbersAvailable(maxRowIndex - 1)
          val found1 =
            findNumbersOverlaping((maxColIndex - 1, maxColIndex - 1), row1)
          val found2 =
            findNumbersOverlaping((maxColIndex - 1, maxColIndex), row2)
          numbersAvailable(maxRowIndex) --= found1
          numbersAvailable(maxRowIndex - 1) --= found2
          numbers ++= (found1 ++ found2)
        else
          val row1 = numbersAvailable(row - 1)
          val row2 = numbersAvailable(row)
          val row3 = numbersAvailable(row + 1)
          val found1 =
            findNumbersOverlaping((col - 1, col + 1), row1)
          val found2L =
            findNumbersOverlaping((col - 1, col - 1), row2)
          val found2R =
            findNumbersOverlaping((col + 1, col + 1), row2)
          val found3 =
            findNumbersOverlaping((col - 1, col + 1), row3)

          numbersAvailable(row - 1) --= found1
          numbersAvailable(row) --= found2L ++ found2R
          numbersAvailable(row + 1) --= found3
          numbers ++= (found1 ++ found2L ++ found2R ++ found3)
      }

      numbers

    def parseEngineSchematics =
      getAdjacentNumbers(_: Array[String]).map(_._2).sum

  object P2:
    import P1._
    def getGearRatios(lines: Array[String]) =
      val gearSymbols = findSymbols(lines).filter(_.symbol == '*')
      val maxRowIndex = lines.length - 1
      val maxColIndex = lines(0).size - 1
      val numberRanges = getNumberRanges(lines)
      val numbersAvailable = numberRanges.zipWithIndex
        .map((ranges, index) =>
          ranges.map(r => (r, rangeToNumber(lines(index), r)))
        )
        .map(mutable.ArrayBuffer.from)
      val numbers = mutable.ListBuffer[(Range, Int)]()
      val gearRatioAdjacencyMap = mutable.Map[Symbol, Seq[Int]]()

      def getNumbersOverlaping(
          coord: (Int, Int),
          row: mutable.Seq[(Range, Int)]
      ) =
        findNumbersOverlaping(coord, row).map(_._2)

      gearSymbols.foreach { symbol =>
        val Symbol(row, col, _) = symbol
        // Top left
        if (row == 0 && col == 0)
          val row1 = numbersAvailable(0)
          val row2 = numbersAvailable(1)
          val found1 = getNumbersOverlaping((1, 1), row1)
          val found2 = getNumbersOverlaping((0, 1), row2)
          val total = (found1 ++ found2).toSeq
          gearRatioAdjacencyMap.put(symbol, total)

        // Top right
        if (row == 0 && col == maxColIndex)
          val row1 = numbersAvailable(0)
          val row2 = numbersAvailable(1)
          val found1 =
            getNumbersOverlaping((maxColIndex - 1, maxColIndex - 1), row1)
          val found2 =
            getNumbersOverlaping((maxColIndex - 1, maxColIndex), row2)
          val total = (found1 ++ found2).toSeq
          gearRatioAdjacencyMap.put(symbol, total)

        // Bottom left
        else if (row == maxRowIndex && col == 0)
          val row1 = numbersAvailable(maxRowIndex)
          val row2 = numbersAvailable(maxRowIndex - 1)
          val found1 =
            getNumbersOverlaping((1, 1), row1)
          val found2 =
            getNumbersOverlaping((0, 1), row2)
          val total = (found1 ++ found2).toSeq
          gearRatioAdjacencyMap.put(symbol, total)

        // Bottom right
        else if (row == maxRowIndex && col == maxColIndex)
          val row1 = numbersAvailable(maxRowIndex)
          val row2 = numbersAvailable(maxRowIndex - 1)
          val found1 =
            getNumbersOverlaping((maxColIndex - 1, maxColIndex - 1), row1)
          val found2 =
            getNumbersOverlaping((maxColIndex - 1, maxColIndex), row2)
          val total = (found1 ++ found2).toSeq
          gearRatioAdjacencyMap.put(symbol, total)
        else
          val row1 = numbersAvailable(row - 1)
          val row2 = numbersAvailable(row)
          val row3 = numbersAvailable(row + 1)
          val found1 =
            getNumbersOverlaping((col - 1, col + 1), row1)
          val found2L =
            getNumbersOverlaping((col - 1, col - 1), row2)
          val found2R =
            getNumbersOverlaping((col + 1, col + 1), row2)
          val found3 =
            getNumbersOverlaping((col - 1, col + 1), row3)

          val total = (found1 ++ found2L ++ found2R ++ found3).toSeq
          gearRatioAdjacencyMap.put(symbol, total)
      }

      gearRatioAdjacencyMap
        .filter((_, list) => list.length == 2)

    def calculateGearRatioSum = getGearRatios(_: Array[String]).values
      .map(seq => seq(0) * seq(1))
      .sum
