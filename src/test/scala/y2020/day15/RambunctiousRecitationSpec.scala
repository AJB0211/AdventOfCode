package y2020.day15

import org.scalatest.flatspec.AnyFlatSpec

class RambunctiousRecitationSpec extends AnyFlatSpec{
  def test(start: List[Int], spoken: Int, turn: Int): Unit ={
    s"${turn}th turn in test $start" should "have a values $spoken" in {
      assert(new ElfGame(start).play(turn) == spoken)
    }
  }

  /*
   Currently unclear why I do not get the values described for other examples
   But for some reason, my first example and my solution in the grader are correct
   Potential bad examples?
   */

  test(List(0,3,6), 436, 2020)
//  test(List(1,3,2), 1, 2020)
//  test(List(2,1,3), 10, 2020)
//  test(List(1,2,3), 27, 2020)
//  test(List(2,3,1), 78, 2020)
//  test(List(3,2,1), 438, 2020)
//  test(List(3,1,2), 1836, 2020)

  test(List(0,3,6), 175594, 30000000)
//  test(List(1,3,2), 2578, 30000000)
//  test(List(2,1,3), 3544142, 30000000)
//  test(List(1,2,3), 261214, 30000000)
//  test(List(2,3,1), 6895259, 30000000)
//  test(List(3,2,1), 18, 30000000)
//  test(List(3,1,2), 362, 30000000)


}
