package net.fearlessmind

import DayEight._

class DayEightTests extends munit.FunSuite:
  test("P1") {
    val input = """
    |RL
    |
    |AAA = (BBB, CCC)
    |BBB = (DDD, EEE)
    |CCC = (ZZZ, GGG)
    |DDD = (DDD, DDD)
    |EEE = (EEE, EEE)
    |GGG = (GGG, GGG)
    |ZZZ = (ZZZ, ZZZ)
    """.stripMargin.trim()

    assertEquals(getPart1(input), 2L)
  }

  test("P1.input") {
    val path = os.pwd / "test" / "resources" / "d8p1.txt"

    println(getPart1(os.read(path)))
  }
  Seq(
    (2, 3L),
    (3, 5L),
    (5, 7L),
    (7, 9L),
    (18, 19L)
  ).foreach { case (current, expected) =>
    test("nextPrime") {
      assertEquals(nextPrime(current), expected)
    }
  }

  test("lcm") {
    assertEquals(lcm(18, 30), 90L)
  }

  test("P2") {
    val input = """
    |LR
    |
    |11A = (11B, XXX)
    |11B = (XXX, 11Z)
    |11Z = (11B, XXX)
    |22A = (22B, XXX)
    |22B = (22C, 22C)
    |22C = (22Z, 22Z)
    |22Z = (22B, 22B)
    |XXX = (XXX, XXX)
    """.stripMargin.trim()

    assertEquals(getPart2(input), 6L)
  }

  test("P2.input") {
    val path = os.pwd / "test" / "resources" / "d8p2.txt"
    try println(getPart2(os.read(path)))
    catch case e: Exception => e.printStackTrace()
  }
