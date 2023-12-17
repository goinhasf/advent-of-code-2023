package net.fearlessmind

import scala.collection.immutable.NumericRange
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArraySeq
import scala.collection.immutable.TreeSet

object DayFive:

  object P1:
    case class MappingEntry(
        source: NumericRange[Long],
        destination: NumericRange[Long]
    )
    type AlmanacItem = Array[MappingEntry]
    extension (item: AlmanacItem)
      def convert(a: Long): Long = item.foldRight(a) { (current, previous) =>
        if (a >= current.source.start && a < current.source.end)
          val index = a - current.source.start
          current.destination.start + index
        else previous
      }

      def convert(range: NumericRange[Long]): Seq[Long] = range
        .map(a =>
          item.foldRight(a) { (current, previous) =>
            if (current.source.contains(a))
              val index = a - current.source.start
              current.destination.start + index
            else previous
          }
        )

    type ConversionF = Function1[AlmanacItem, Long]

    def applyConversions(initial: Long, arr: Array[AlmanacItem]): Long =
      arr.reverse.foldRight(initial) { (current, previous) =>
        current.convert(previous)
      }

    def parseInput(input: String): (Array[Long], Array[AlmanacItem]) =
      val inputByNewLine = input.split("\\n\\n").map(_.trim)
      val seeds =
        inputByNewLine.head.replace("seeds: ", "").split(" ").map(_.toLong)
      val rest = inputByNewLine.tail

      val almanac = rest.map { textEntry =>
        textEntry
          .split("\\n")
          .tail
          .map(_.split(" ").map(_.toLong))
          .map { case Array(destination, source, length) =>
            MappingEntry(
              Range.Long(source, source + length, 1),
              Range.Long(destination, destination + length, 1)
            )
          }

      }

      (seeds, almanac)

    def findLocation(input: String) =
      val (seeds, almanac) = parseInput(input)
      seeds.map(seed => applyConversions(seed, almanac)).sorted.head

  object P2:

    case class MappingEntry(
        source: Long,
        destination: Long,
        size: Long
    )

    type AlmanacItem = Array[MappingEntry]
    extension (item: AlmanacItem)
      def applyRange(range: Seq[(Long, Long)]) =
        val A = ListBuffer[(Long, Long)]()
        var R = ListBuffer.from(range)

        for (x <- item)
          val MappingEntry(dest, src, sz) = x
          val src_end = src + sz
          val NR = ListBuffer[(Long, Long)]()
          for r <- R do
            val (st, ed) = r
            val before = (st, Math.min(ed, src))
            val inter = (Math.max(st, src), Math.min(src_end, ed))
            val after = (Math.max(src_end, st), ed)

            if before(1) > before(0) then NR += (before)
            if inter(1) > inter(0) then
              A += ((inter(0) - src + dest, inter(1) - src + dest))
            if after(1) > after(0) then NR.append(after)

          R = NR
        val result = A ++ R
        result.toSeq

    case class Input(seeds: Array[(Long, Long)], items: Array[AlmanacItem])

    def parseInput(
        input: String
    ): Input =
      val inputByNewLine = input.split("\\n\\n").map(_.trim)
      val r = "\\d+ \\d+".r
      val seeds = r
        .findAllMatchIn(
          inputByNewLine.head
            .replace("seeds: ", "")
        )
        .toArray
        .map(_.matched)
        .map(_.split(" "))
        .map(_.map(_.toLong))
        .map(arr => (arr(0), arr(0) + arr(1)))

      val rest = inputByNewLine.tail

      val almanac = rest.map { textEntry =>
        textEntry
          .split("\\n")
          .tail
          .map(_.split(" ").map(_.toLong))
          .map { case Array(destination, source, length) =>
            MappingEntry(
              source,
              destination,
              length
            )
          }

      }

      Input(seeds, almanac)
    end parseInput

    def map(
        range: Seq[(Long, Long)],
        items: Array[MappingEntry]
    ): Seq[(Long, Long)] =
      val nr = ListBuffer[(Long, Long)]()

      for {
        item <- items
        seed <- range
      } yield {
        val (seedStart, seedEnd) = seed
        val MappingEntry(source, destination, size) = item
        val (itemStart, itemEnd) = (source, source + size)
        val (destinationStart, destinationEnd) =
          (destination, destination + size)

        val before = (seedStart, Math.min(seedEnd, itemStart))
        val middle =
          (Math.max(seedStart, itemStart), Math.min(itemEnd, seedEnd))
        val after = (Math.max(itemEnd, seedStart), seedEnd)

        if (before(1) > before(0))
          nr += before
        if (middle(1) > middle(0))
          nr += ((
            middle(0) - itemStart + destinationStart,
            middle(1) - itemStart + destinationStart
          ))
        if (after(1) > after(0))
          nr += after

      }
      Seq.from(nr)
    end map

    def findLocation(input: String): Long =
      val Input(seeds, items) = parseInput(input)
      var min = ListBuffer[Long]()

      for (seed <- seeds) {
        var ls = Seq[(Long, Long)](seed)
        for (item <- items) {
          ls = item.applyRange(ls)

        }
        val r = ls.map(_(0)).min
        min += r
      }

      println(min)
      min.min
