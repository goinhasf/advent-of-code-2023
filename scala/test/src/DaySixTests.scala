package net.fearlessminds
import DaySix._

class DaySixTests extends munit.FunSuite:
  test("P1") {
    import P1._
    val input = """
    |Time:      7  15   30
    |Distance:  9  40  200
    """.stripMargin.trim()

    assertEquals(
      calculate(input),
      288L
    )
  }

  test("P1.input") {
    import P1._
    val input = """
    |Time:        34     90     89     86
    |Distance:   204   1713   1210   1780
    """.stripMargin.trim()

    println(calculate(input))
  }

  test("P2") {
    import P1._
    val input = """
    |Time:      71530 
    |Distance:  940200
    """.stripMargin.trim()

    assertEquals(
      calculate(input),
      71503L
    )
  }

  test("P2.input") {
    import P2._
    val input = """
    |Time:        34     90     89     86
    |Distance:   204   1713   1210   1780
    """.stripMargin.trim()

    println(
      calculate(input)
    )
  }
