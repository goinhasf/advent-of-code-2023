package net.fearlessmind

import DayFive._
import DayFive.P1._
import scala.collection.immutable.NumericRange

class DayFiveTests extends munit.FunSuite:

  val input = """
    |seeds: 79 14 55 13
    |
    |seed-to-soil map:
    |50 98 2
    |52 50 48
    |
    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15
    |
    |fertilizer-to-water map:
    |49 53 8
    |0 11 42
    |42 0 7
    |57 7 4
    |
    |water-to-light map:
    |88 18 7
    |18 25 70
    |
    |light-to-temperature map:
    |45 77 23
    |81 45 19
    |68 64 13
    |
    |temperature-to-humidity map:
    |0 69 1
    |1 0 69
    |
    |humidity-to-location map:
    |60 56 37
    |56 93 4
    """.stripMargin.trim()
  Seq(
    (79, 79L),
    (98, 50L),
    (99, 51L),
    (100, 50L)
  ).foreach((input, expected) =>
    test("convert") {
      val seedToSoil = Array(
        MappingEntry(
          Range.Long(98, 98 + 2, 1),
          Range.Long(50, 50 + 2, 1)
        ),
        MappingEntry(
          Range.Long(100, 100 + 2, 1),
          Range.Long(50, 50 + 2, 1)
        )
      )

      val obtained = P1.applyConversions(
        input,
        Array(seedToSoil)
      )

      assertEquals(obtained, expected)
    }
  )

  test("P1.findLocation") {
    assertEquals(P1.findLocation(input), 35L)
  }

  test("P1.findLocation - input") {
    val path = os.pwd / "test" / "resources" / "d5p1.txt"
    val obtained = P1.findLocation(os.read(path))

    println(obtained)
  }

  test("P2.findLocation") {
    assertEquals(P2.findLocation(input), 46L)
  }

  test("P2.findLocationInput") {
    try
      val path = os.pwd / "test" / "resources" / "d5p2.txt"
      val obtained = P2.findLocation(os.read(path))
      println(obtained)
    catch case e: OutOfMemoryError => e.printStackTrace()
  }
