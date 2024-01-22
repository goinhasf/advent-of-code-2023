package net.fearlessmind

import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

object DayEight:
  case class Input(instructions: String, state: Map[String, (String, String)])

  def parseInput(input: String): Input =
    val Array(instructions, rest) = input.split("\n\n")

    val state =
      rest
        .split("\n")
        .map { line =>
          val Array(left, right) = line.replace(" ", "").split("=")

          val Array(a, b) =
            "[0-9A-Z]{3}".r.findAllMatchIn(right).map(_.matched).toArray

          left -> ((a, b))
        }
        .toMap

    Input(instructions, state)

  def calculate(input: Input): Long =
    val Input(instructions, state) = input

    var counter = 0L
    var currentElement = "AAA"
    var index = 0

    while currentElement != "ZZZ" do
      if (index >= instructions.size)
        index = 0

      val instruction = instructions(index)
      index += 1

      val next = state(currentElement)

      currentElement = instruction match
        case 'L' => next(0)
        case 'R' => next(1)

      counter += 1

    counter
  end calculate

  def nextPrime(start: Long = 2) =
    def rec(current: Long, found: Option[Long]): Long =
      if found.isEmpty then current
      else rec(current + 1, (start until current).find(x => current % x == 0))
    rec(start, Some(start))

  def getFrequencyMap(seq: Seq[Long]) = seq.foldRight(Map.empty[Long, Int]) {
    (current, previous) =>
      previous + (current -> seq.count(_ == current))
  }

  def findPrimes(num: Long) =
    var acc = mutable.ListBuffer.empty[Long]
    var current = num
    var previousPrime = 2L
    while current != 1 do
      if (current % previousPrime == 0)
        acc += previousPrime
        current = current / previousPrime
        previousPrime = 2L
      previousPrime = nextPrime(previousPrime)
    acc.toSeq

  def lcm(a: Long, b: Long, primesA: Seq[Long], primesB: Seq[Long]): Long =
    val frequencyMapA = getFrequencyMap(primesA)
    val frequencyMapB = getFrequencyMap(primesB)

    val aAndB = frequencyMapB.keys.foldRight(Map.empty[Long, Int]) {
      (current, previous) =>
        frequencyMapA.get(current) match
          case None => previous
          case Some(value) => {
            val countInB = frequencyMapB(current)
            val min = Math.min(value, countInB)
            previous + (current -> min)
          }

    }

    val hcm = aAndB.foldRight(1L) { (current, previous) =>
      val (num, count) = current
      previous * Math.pow(num, count).toLong
    }

    (a * b) / hcm

  def lcm(a: Long, b: Long): Long =
    val primesA = findPrimes(a)
    val primesB = findPrimes(b)
    lcm(a, b, primesA, primesB)

  def lcm(nums: Seq[Long]): Long =
    if nums.length < 2 then
      throw new IllegalArgumentException("Seq must have at least two numbers")
    if nums.length == 2 then lcm(nums(0), nums(1))
    else lcm(Seq(lcm(nums(0), nums(1))) ++ nums.drop(2))

  def calculateP2(input: Input): Long =
    val Input(instructions, state) = input
    val startNodes = state.keySet.filter(_.endsWith("A")).toList

    def findLoopLength(node: String) =
      val visits = mutable.Map.empty[String, Int]
      val loop = mutable.ListBuffer.empty[String]
      var index = 0
      var isLoop = false
      var countLoop = false
      var currentNode = node

      while !isLoop do

        if countLoop then loop += currentNode

        visits.get(currentNode) match
          case None => visits(currentNode) = 1
          case Some(value) => {
            val newValue = value + 1
            if (currentNode.endsWith("Z"))
              if (newValue == 2) countLoop = true
              if (newValue == 3) isLoop = true

            visits(currentNode) = newValue

          }

        if (index >= instructions.size)
          index = 0

        val instruction = instructions(index)
        index += 1

        val nextNode = instruction match
          case 'L' => state(currentNode)(0)
          case 'R' => state(currentNode)(1)

        currentNode = nextNode
      loop.length
    end findLoopLength

    val loopLengths = startNodes.map(findLoopLength).map(_.toLong)
    println(loopLengths)
    lcm(loopLengths)

  end calculateP2

  def getPart1 = calculate.compose(parseInput)
  def getPart2 = calculateP2.compose(parseInput)
