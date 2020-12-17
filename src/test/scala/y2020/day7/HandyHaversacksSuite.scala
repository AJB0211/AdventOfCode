package y2020.day7

import ajb0211.Advent.util.readFile
import org.scalatest.funsuite.AnyFunSuite

class HandyHaversacksSuite extends AnyFunSuite{
  def bags = readFile("y2020/7_test.txt")
  def bags2 = readFile("y2020/7_test2.txt")

  val graph = new SackGraph(bags)
  val graph2 = new SackGraph(bags2)

  test("Shiny gold should have 4 parents") {assert(graph.countParents("shiny gold") == 4)}

  test("Shiny gold should be able to contain 32 bags in first example") {
    assert(graph.countChildren("shiny gold") == 32)
  }

  test("Shiny gold should be able to contain 126 bags in second example") {
    assert(graph2.countChildren("shiny gold") == 126)
  }
}
