package net.fearlessmind

import DaySeven._

class DaySevenTests extends munit.FunSuite:
  test("P1") {
    val input = """
    |32T3K 765
    |T55J5 684
    |KK677 28
    |KTJJT 220
    |QQQJA 483
    """.stripMargin.trim()

    assertEquals(P1.calculateWinnings(input), 6440L)
  }

  test("P1.input") {
    val path = os.pwd / "test" / "resources" / "d7p1.txt"
    val input = os.read(path)

    println(P1.calculateWinnings(input))
  }

  test("P2") {
    import P2.given_HandApply

    val input = """
    |32T3K 765
    |T55J5 684
    |KK677 28
    |KTJJT 220
    |QQQJA 483
    """.stripMargin.trim()

    assertEquals(P2.calculateWinnings(input), 5905L)
  }

  test("P2.input") {
    val path = os.pwd / "test" / "resources" / "d7p2.txt"
    val input = os.read(path)

    println(P2.calculateWinnings(input))
  }
