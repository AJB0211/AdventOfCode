package y2020.day13

import org.scalatest.flatspec.AnyFlatSpec

class ShuttleSearchSpec extends AnyFlatSpec {
  val schedule: Schedule = Schedule("y2020/13_test.txt")

  "Earliest bus" should "give value 295" in {
    assert(schedule.earliest == 295)
  }

  "Earliest defined sequence" should "be at 1068781" in {
    assert(schedule.minSeries == 1068781)
  }

}
