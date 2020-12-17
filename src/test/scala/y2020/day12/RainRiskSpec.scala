package y2020.day12

import ajb0211.Advent.util.readFile
import org.scalatest.flatspec.AnyFlatSpec
import y2020.day12.RainRisk.actions

class RainRiskSpec extends AnyFlatSpec {
  def actions: Iterator[Action] = readFile("y2020/12_test.txt").map(Action(_))

  val part1 = actions.foldLeft(Ship(1,0,0)) {
    case (ship: Ship, action: Action) =>
      ship.move(action)
  }

  val part2 = actions.foldLeft(new ShipSystem()) {
    case (ss: ShipSystem, action: Action) =>
      ss.move(action)
  }

  "Manhattan distance with ship movement" should "be 25" in {
    assert(part1.manhattan() == 25)
  }

  "Manhattan distance with ship-waypoint system" should "be 286" in {
    assert(part2.manhattan == 286)
  }

}
