package y2020.day10

import ajb0211.Advent.util.readFile
import org.scalatest.flatspec.AnyFlatSpec

class AdapterArraySpec extends AnyFlatSpec{
  def adapters: Array[Int] = readFile("y2020/10_test.txt").map(_.toInt).toArray
  def adapters2: Array[Int] = readFile("y2020/10_test2.txt").map(_.toInt).toArray


  "Product of test 1 differences" should "be 35" in {
    assert(AdapterArray.joltDifferences(adapters) == 35)
  }

  "Product of test 2 differences" should "be 220" in {
    assert(AdapterArray.joltDifferences(adapters2) == 220)
  }

  "Number of test 1 arrangements" should "be 8" in {
    assert(AdapterArray.joltCombinations(adapters) == 8)
  }

  "Number of test 2 arrangements" should "be 19208" in {
    assert(AdapterArray.joltCombinations(adapters2) == 19208)
  }
}
