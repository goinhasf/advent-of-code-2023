package net.fearlessmind

object DayTwo:
  case class GameScore(r: Int = 0, g: Int = 0, b: Int = 0)
  case class Game(id: Int, scores: Seq[GameScore])

  object P1:
    extension (score: GameScore)
      def +(other: GameScore) = GameScore(
        r = score.r + other.r,
        g = score.g + other.g,
        b = score.b + other.b
      )

    def parseLine(line: String): Game =
      val splitLine = line.replace("Game ", "").split(": ")
      val gameNumber = splitLine.head.toInt
      val gameSets = splitLine(1).split("; ").map(_.split(", "))
      val scores = gameSets.map { score =>
        score.foldRight(GameScore()) { (c, p) =>
          val scoreArr = c.split(" ")
          val num = scoreArr.head.toInt
          val colour = scoreArr(1)

          colour match
            case "red"   => GameScore(r = num) + p
            case "green" => GameScore(g = num) + p
            case "blue"  => GameScore(b = num) + p
        }
      }

      Game(gameNumber, scores.toSeq)

    def isGamePossible(game: Game) =
      game.scores.forall {
        case GameScore(r, g, b) if (r > 12 || g > 13 || b > 14) => false
        case _                                                  => true
      }

    def parseGame(input: Array[String]) = input
      .map(parseLine)

    def sumOfGamesIds(input: Array[String]) = input
      .map(parseLine)
      .filter(isGamePossible)
      .map(_.id)
      .reduce(_ + _)

  object P2:
    def findMax(game: Game): GameScore =
      game.scores.foldRight(GameScore()) { (current, previous) =>
        var r = previous.r
        var g = previous.g
        var b = previous.b

        if (current.r > r) {
          r = current.r
        }

        if (current.g > g) {
          g = current.g
        }

        if (current.b > b) {
          b = current.b
        }

        GameScore(r, g, b)
      }

    def powerOfMinimumSetOfCubes(score: GameScore) = score.r * score.g * score.b

    def sumOfPowers(input: Array[String]) = P1
      .parseGame(input)
      .map(findMax)
      .map(powerOfMinimumSetOfCubes)
      .reduce(_ + _)
