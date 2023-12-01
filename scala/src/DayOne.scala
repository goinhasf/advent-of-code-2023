package net.fearlessmind

object DayOne:
  object P1:
    def findCalibrationValue(input: String): Int =
      val arr = input.filter(_.isDigit)
      if (arr.isEmpty()) 0
      else s"${arr.head}${arr.last}".toInt

    def sumOfCalibrationValues(input: Array[String]): Int =
      input
        .map(_.replace("[A-Za-z]", ""))
        .map(findCalibrationValue)
        .reduce(_ + _)

  object P2:
    val numbers = Array(
      ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    )

    def replaceWordsWithNums(input: Array[String]): Array[String] =
      input.map(s =>
        numbers.foldLeft(s) { (p, c) =>
          val (word, num) = c
          p.replace(word, s"$word${num.toString()}$word")
        }
      )

    def sumOfCalibrationValuesWithReplacement(input: Array[String]): Int =
      val replaced = replaceWordsWithNums(input)
      P1.sumOfCalibrationValues(replaced)
