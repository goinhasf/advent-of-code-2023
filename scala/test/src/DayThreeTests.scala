package net.fearlessmind

import scala.collection.mutable
import DayThree._
import DayThree.P1._
import DayThree.P2._

class DayThreeTests extends munit.FunSuite:
  val input = """
    |467..114..
    |...*......
    |..35..633.
    |......#...
    |617*......
    |.....+.58.
    |..592.....
    |......755.
    |...$.*....
    |.664.598..
    """.stripMargin
    .trim()
    .split("\n")

  Seq(
    (
      Seq(
        ".$...",
        "....."
      ),
      Seq(Symbol(0, 1, '$'))
    ),
    (
      Seq(
        ".$...",
        "...%."
      ),
      Seq(Symbol(0, 1, '$'), Symbol(1, 3, '%'))
    ),
    (
      Seq(
        ".$45.",
        ".%.$."
      ),
      Seq(Symbol(0, 1, '$'), Symbol(1, 1, '%'), Symbol(1, 3, '$'))
    )
  ).foreach { case (lines, expected) =>
    test(s"findSymbols $lines") {
      val obtained = findSymbols(lines)
      assertEquals(obtained, expected)
    }
  }

  Seq(
    ((0, 0), mutable.Seq((Range(0, 1), 12)), mutable.Seq(12)),
    (
      (1, 2),
      mutable.Seq((Range(1, 1).inclusive, 1), (Range(1, 2).inclusive, 23)),
      mutable.Seq(1, 23)
    )
  ).foreach { (coordinates, line, expected) =>
    test(s"findNumbersOverlaping") {
      val obtained = findNumbersOverlaping(coordinates, line).map(_._2)
      assertEquals(obtained, expected)

    }
  }

  test(s"parseEngineSchematic") {
    val input = """
    |467..114..
    |...*......
    |..35..633.
    |......#...
    |617*......
    |.....+.58.
    |..592.....
    |......755.
    |...$.*....
    |.664.598..
    """.stripMargin
      .trim()
      .split("\n")

    val obtained = parseEngineSchematics(input)

    assertEquals(obtained, 4361)
  }

  test("parseEngineSchematic - input") {
    val path = os.pwd / "test" / "resources" / "d3p1.txt"
    val obtained = parseEngineSchematics(os.read.lines(path).toArray)

    println(obtained)

  }

  test("calculateGearRatioSum") {
    val input = """
    |467..114..
    |...*......
    |..35..633.
    |......#...
    |617*......
    |.....+.58.
    |..592.....
    |......755.
    |...$.*....
    |.664.598..
    """.stripMargin
      .trim()
      .split("\n")

    val obtained = calculateGearRatioSum(input)
    assertEquals(obtained, 467835)
  }

  test("calculateGearRatioSum - input") {
    val path = os.pwd / "test" / "resources" / "d3p2.txt"
    val obtained = calculateGearRatioSum(os.read.lines(path).toArray)

    println(obtained)
  }
