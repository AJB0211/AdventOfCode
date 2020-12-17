package y2020.day14

import ajb0211.Advent.util.readFile
import org.scalatest.flatspec.AnyFlatSpec

import y2020.day14.Register.{Version1, Version2}

class DockingDataSpec extends AnyFlatSpec {

  private def input = readFile("y2020/14_test.txt")

  "Version 1 test" should "produce a sum of 165" in {
    assert(Version1.run(input).readSum == 165)
  }

  "Version 2 test" should "produce a sum of 208" in {
    assert(Version2.run(input).readSum == 208)
  }

}
