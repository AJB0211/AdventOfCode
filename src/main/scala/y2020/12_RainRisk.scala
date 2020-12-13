package ajb0211.Advent.y2020

import ajb0211.Advent.util.readFile

import scala.annotation.tailrec

object RainRisk extends App {
  // def makes it so actions are returned each time it's called
  // convenient so the iterator doesn't deplete itself
  def actions: Iterator[Action] = readFile("y2020/12.txt").map(Action(_))

  val part1 = actions.foldLeft(Ship(1,0,0)) {
    case (ship: Ship, action: Action) =>
      ship.move(action)
  }

  val part2 = actions.foldLeft(new ShipSystem()) {
    case (ss: ShipSystem, action: Action) =>
      ss.move(action)
  }

  println("Part 1: Instruction set for moving ship")
  println(part1.manhattan())
  println
  println("Part 2: Instruction for ship-waypoint system")
  println(part2.manhattan)
}

/**
 * Representation of directions in input set
 * Provides access to case matching for clean syntax
 */
sealed abstract class Action
case class North(units: Int) extends Action
case class South(units: Int) extends Action
case class East(units: Int) extends Action
case class West(units: Int) extends Action
case class Left(units: Int) extends Action
case class Right(units: Int) extends Action
case class Forward(Units: Int) extends Action

/**
 * Class object to parse lines into corresponding actions
 */
object Action{
  def apply(line: String): Action = {
    val Line = raw"(\D)(\d+)".r
    line match {
      case Line(d, n) if d(0) == 'N' => North(n.toInt)
      case Line(d, n) if d(0) == 'S' => South(n.toInt)
      case Line(d, n) if d(0) == 'E' => East(n.toInt)
      case Line(d, n) if d(0) == 'W' => West(n.toInt)
      case Line(d, n) if d(0) == 'L' => Left(n.toInt)
      case Line(d, n) if d(0) == 'R' => Right(n.toInt)
      case Line(d, n) if d(0) == 'F' => Forward(n.toInt)
    }
  }
}

/**
 * Manages the logic of a ship in Part 1
 * 0 : North
 * 1 : East
 * 2 : South
 * 3 : West
 * @param direction cardinal direction the ship is facing
 * @param x x coordinate of ship location
 * @param y y coordinate of ship location
 */
case class Ship(direction: Int, x: Int, y: Int){
  /**
   * Manhattan distance from reference point
   * @param ref reference point, as a ship, defaults to origin
   * @return distance
   */
  def manhattan(ref: Ship = Ship(1,0,0)): Int = math.abs(x - ref.x) + math.abs(y - ref.y)

  /**
   * Update rule for direction field when the ship is turned
   * @param theta input angle to turn
   * @return resulting heading after turns
   */
  private def turn(theta: Int): Int = (4 + ((direction + (theta/90)) % 4)) % 4

  def move(action: Action): Ship = action match {
    case Forward(du) =>
      if      (direction == 0) Ship(direction, x, y+du)
      else if (direction == 1) Ship(direction, x+du, y)
      else if (direction == 2) Ship(direction, x, y-du)
      else if (direction == 3) Ship(direction, x-du, y)
      else throw new Exception("Invalid Heading")
    case North(dy) => Ship(direction, x, y+dy)
    case South(dy) => Ship(direction, x, y-dy)
    case East(dx) => Ship(direction, x+dx, y)
    case West(dx) => Ship(direction, x-dx, y)
      // dT for delta theta
    case Left(dT) =>  Ship(turn(-1*dT),x,y)
    case Right(dT) => Ship(turn(dT),x,y)
  }

}

/**
 * Container for a ship in Part 2
 * @param x x coordinate of ship location
 * @param y y coordinate of ship location
 */
case class Point(x: Int, y:Int)

/**
 * Manages the logic of a Waypoint
 * @param x x coordinate of waypoint location
 * @param y y coordinate of waypoint location
 */
case class Waypoint(x: Int, y: Int){
  /**
   * From rotation matrix at angle -pi/2 in 2D system
   * x -> y
   * y -> -x
   * @param n number of turns
   * @return Waypoint after rotations
   */
  @tailrec
  private def turnLeft(n: Int): Waypoint =
    if (n <= 0) this
    else Waypoint(-1*y, x).turnLeft(n-1)

  /**
   * From rotation matrix at angle pi/2 in 2D system
   * x -> -y
   * y -> x
   * @param n number of turns
   * @return Waypoint after rotations
   */
  @tailrec
  private def turnRight(n: Int): Waypoint =
    if (n <= 0) this
    else Waypoint(y, -1*x).turnRight(n-1)

  def move(action: Action): Waypoint = action match {
    case Forward(_) => this
    case North(dy) => Waypoint(x, y+dy)
    case South(dy) => Waypoint(x, y-dy)
    case East(dx) => Waypoint(x+dx, y)
    case West(dx) => Waypoint(x-dx, y)
    // dT for delta theta
    case Left(dT) =>  turnLeft(dT/90)
    case Right(dT) => turnRight(dT/90)
  }

}

/**
 * To manage ship-waypoint system in Part 2
 * @param ship location of ship, as a Point(x,y)
 * @param waypoint location of waypoint, as a Waypoint(x,y)
 */
class ShipSystem(val ship: Point = Point(0,0), val waypoint: Waypoint = Waypoint(10,1)){
  /**
   * Only the forward action needs to be intercepted
   * Otherwise, the action can be delegated to Waypoint.move
   * @param action see Action sealed case class
   * @return ShipSystem after action
   */
  def move(action: Action): ShipSystem = action match {
    case Forward(du) =>
      if (du <= 0) this
      else new ShipSystem(
        Point(ship.x + waypoint.x, ship.y + waypoint.y),
        waypoint
    ).move(Forward(du-1))
    case other: Action => new ShipSystem(
      ship,
      waypoint.move(other)
    )
  }

  /**
   * Finds location of ship in Manhattan distance from origin
   * @return distance
   */
  def manhattan: Int = math.abs(ship.x) + math.abs(ship.y)
}