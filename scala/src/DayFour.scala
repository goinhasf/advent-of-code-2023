package net.fearlessmind

import collection.mutable

object DayFour:
  case class InputLine(
      index: Int,
      winningNumbers: Seq[Int],
      obtainedNumbers: Seq[Int]
  )

  object P1:
    def getScoreFromCard(matchedScores: Seq[Int]): Int =
      val matched = matchedScores.length
      if (matched == 0) 0
      else Math.pow(2.toDouble, matched - 1).toInt

    def parseLine(line: String): InputLine =
      val (left, obtainedNumbersString) = line.splitAt(line.indexOf(" | "))
      val (cardName, winningNumbersString) = left.splitAt(line.indexOf(": "))
      val d = "\\d+".r

      val obtainedNumbers =
        d.findAllMatchIn(obtainedNumbersString).map(_.matched.toInt).toSeq
      val winningNumbers =
        d.findAllMatchIn(winningNumbersString).map(_.matched.toInt).toSeq

      val index = "\\d+".r.findFirstIn(cardName).get.toInt

      InputLine(index - 1, winningNumbers, obtainedNumbers)

    def parseInput(lines: Array[String]) = lines.map(parseLine)

    def getWinningNumbers(input: InputLine) =
      input.winningNumbers.intersect(input.obtainedNumbers)

    def calculateScore(lines: Array[String]) =
      parseInput(lines)
        .map(getWinningNumbers)
        .map(getScoreFromCard)
        .sum

  object P2:
    import P1._

    def calculateScoreWithNewRules(input: Array[String]) =
      val cards = parseInput(input)

      val cardCount = mutable.ArraySeq.from(Array.fill(cards.length)(1))

      for (line <- cards)
        val score = getWinningNumbers(line).length
        if (score != 0)
          val start = line.index + 1
          val end = start + score - 1
          (start to end).foreach(index =>
            cardCount(index) += cardCount(line.index)
          )

      cardCount.sum
