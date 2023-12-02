package net.fearlessmind

class DayTwoTests extends munit.FunSuite:
  import DayTwo._
  import DayTwo.P1._
  import DayTwo.P2._

  test("parseLine") {
    val obtained = parseLine(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    )

    val score = Array(
      GameScore(b = 3, r = 4),
      GameScore(r = 1, g = 2, b = 6),
      GameScore(g = 2)
    )

    assertEquals(obtained, Game(1, score.toSeq))
  }

  test("sumOfGames") {
    val input = Array(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    assertEquals(sumOfGamesIds(input), 8)
  }

  test("sumOfGames with input") {

    val path = os.pwd / "test" / "resources" / "d2p1.txt"
    val obtained = sumOfGamesIds(os.read.lines(path).toArray)

    println(obtained)
  }

  test("findMax") {
    val obtained = findMax(
      Game(
        1,
        Seq(
          GameScore(b = 3, r = 4),
          GameScore(r = 1, g = 2, b = 6),
          GameScore(g = 2)
        )
      )
    )

    assertEquals(obtained, GameScore(r = 4, g = 2, b = 6))
  }

  test("sumOfPowers") {

    val input = Array(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    assertEquals(sumOfPowers(input), 2286)
  }

  test("sumOfPowers with input") {
    val path = os.pwd / "test" / "resources" / "d2p2.txt"
    val obtained = sumOfPowers(os.read.lines(path).toArray)

    println(obtained)
  }
