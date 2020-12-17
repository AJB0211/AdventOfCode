package y2020.day6

import org.scalatest.funsuite.AnyFunSuite
import CustomCustoms.{countAll, countAny}
import ajb0211.Advent.util.readFile

class CustomCustomsSuite extends AnyFunSuite{
  def papers: Iterator[String] = readFile("y2020/6_test.txt")

  test("Sum of any counts should be 11") {assert(countAny(papers) == 11)}
  test("Sum of all counts should be 6") {assert(countAll(papers) == 6)}
}
