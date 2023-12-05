package net.fearlessmind

import DayFour._
import DayFour.P1._
import net.fearlessmind.DayFour.P2.calculateScoreWithNewRules

class DayFourTests extends munit.FunSuite:
  Seq(
    (Range(0, 0), 0),
    (Range(0, 1), 1),
    (Range(0, 2), 2),
    (Range(0, 3), 4),
    (Range(0, 4), 8),
    (Range(0, 5), 16)
  ).foreach((input, expected) =>
    test("countScore") {
      val obtained = getScoreFromCard(input)
      assertEquals(obtained, expected)
    }
  )

  test("parseLine") {
    val input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

    val obtained = parseLine(input)

    assertEquals(
      obtained,
      InputLine(0, Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53))
    )

  }

  test("calculateScore") {
    val input = """
    $Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    $Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    $Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    $Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    $Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    $Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """.stripMargin('$').trim().split("\n")

    val obtained = calculateScore(input)

    assertEquals(obtained, 13)
  }

  test("calculateScore - input") {
    val path = os.pwd / "test" / "resources" / "d4p1.txt"
    val obtained = calculateScore(os.read.lines(path).toArray)

    println(obtained)
  }

  test("calculateScoreWithNewRules") {
    val input = """
    $Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    $Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    $Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    $Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    $Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    $Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """.stripMargin('$').trim().split("\n")

    val obtained = calculateScoreWithNewRules(input)

    assertEquals(obtained, 30)
  }

  test("calculateScoreWithNewRules - input") {
    val path = os.pwd / "test" / "resources" / "d4p2.txt"
    val obtained = calculateScoreWithNewRules(os.read.lines(path).toArray)

    println(obtained)
  }
