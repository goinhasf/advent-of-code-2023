package net.fearlessminds

object DaySix:
  object P1:
    case class Race(time: Long, distance: Long)
    case class Input(races: Seq[Race])

    def parseInput(input: String): Input =
      val Array(times, distances) =
        input.split("\n").map { line =>
          "\\d+".r.findAllMatchIn(line).map(_.matched).map(_.toLong).toArray
        }

      Input(times.zip(distances).map(Race.apply.tupled))

    def calculateInput(input: Input) =
      val Input(races) = input
      var result = 1L

      for race <- races do
        val minDistance = race.distance
        var currentVelocity = race.time - 1
        var waysOfWinning = 0L

        while currentVelocity >= 0 do
          val travelled =
            currentVelocity * race.time - Math.pow(currentVelocity, 2)
          if travelled > minDistance then waysOfWinning += 1
          currentVelocity -= 1
        end while

        result *= waysOfWinning
      end for

      result
    end calculateInput

    def calculate = calculateInput.compose(parseInput)

  object P2:
    def parseInput(input: String): P1.Input =
      val Array(times, distances) =
        input.split("\n").map { line =>
          "\\d+".r
            .findAllMatchIn(line.replace(" ", ""))
            .map(_.matched)
            .map(_.toLong)
            .toArray
        }
      P1.Input(times.zip(distances).map(P1.Race.apply.tupled))
    end parseInput

    def calculate = P1.calculateInput.compose(parseInput)
