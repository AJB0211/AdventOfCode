package y2020.day17

import org.scalatest.flatspec.AnyFlatSpec

class ConwayCubesSpec extends AnyFlatSpec {
  "A cube" should "find 26 adjacent cubes" in {
    assert(Cube(0,0,0).getAdjacent.size == 26)
  }

  "Part 1 test" should "find 112 cubes in the active state" in {
    assert(Life("y2020/17_test.txt").run(6).numActive == 112)
  }

  "Part 2 test" should "find 848 hypercubes in the active state" in {
    assert(HyperLife("y2020/17_test.txt").run(6).numActive == 848)
  }
}
