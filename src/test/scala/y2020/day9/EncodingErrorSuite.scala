package y2020.day9

import ajb0211.Advent.util.readFile
import org.scalatest.funsuite.AnyFunSuite

class EncodingErrorSuite extends AnyFunSuite{
  def encoding: Iterator[Int] = readFile("y2020/9_test.txt").map(_.toInt)
  val preambleLength = 5

  val jfc = new XMAS(encoding, preambleLength)
  val part1 = jfc.firstInvalid

  test("First invalid element should be 127") {assert(part1 == 127)}

  test("Encryption weakness should be 62") {assert(jfc.sumRange(part1) == 62)}
}
