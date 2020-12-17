package y2020.day11

import org.scalatest.flatspec.AnyFlatSpec

class SeatingSystemSpec extends AnyFlatSpec{
  val ferry = new Ferry("y2020/11_test.txt")
  val ferry2 = new Ferry("y2020/11_test.txt")
  ferry.relax
  ferry2.relax2

  "Ferry with simple seating rules" should "have 37 occupied seats" in {
    assert(ferry.numOccupied == 37)
  }

  "Ferry with extended seating rules" should "have 26 occupied seats" in {
    assert(ferry2.numOccupied == 26)
  }

}
