package net.fearlessmind

class DayOneTests extends munit.FunSuite:
  test("findCalibrationValue") {
    val obtained = DayOne.P1.findCalibrationValue("1abc2")
    assertEquals(obtained, 12)
  }

  test("sumOfCalibrationValues") {

    val obtained = DayOne.P1.findCalibrationValue("1abc2")
    assertEquals(obtained, 12)
  }

  test("sumOfCalibrationValues") {

    val obtained = DayOne.P1.sumOfCalibrationValues(
      Array("1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet", "one2adfsfour")
    )

    assertEquals(obtained, 164)
  }

  test("p1") {
    val path = os.pwd / "test" / "resources" / "d1p1.txt"

    val obtained = DayOne.P1.sumOfCalibrationValues(
      os.read.lines(path).toArray
    )

    println(obtained)
  }

  test("p2") {
    val path = os.pwd / "test" / "resources" / "d1p1.txt"

    val obtained = DayOne.P2.sumOfCalibrationValuesWithReplacement(
      os.read.lines(path).toArray
    )

    println(obtained)
  }
