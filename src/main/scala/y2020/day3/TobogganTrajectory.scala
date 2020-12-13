package y2020.day3

import ajb0211.Advent.util.readFile

object TobogganTrajectory extends App {
  type Hill = Array[Array[Int]]

  /**
   * Constructor in object that takes
   *
   * @param path
   * @return TobogganTrajectory with path information loaded as a Hill
   */
  def apply(path: String): TobogganTrajectory = {
    new TobogganTrajectory(readHill(path))
  }

  /**
   *  Reads a text file and maps values to integers consistent with problem definition
   *     . = open space => 0
   *     # = tree       => 1
   *  We are counting the number of trees so it is convenient to store these as 1
   *
   * @param path file in resources folder containing data
   * @return Hill, see type
   */
  def readHill(path: String): Hill = readFile(path).toArray.map{ _.map{
      (c: Char) => if (c=='#') 1 else 0
      }.toArray
  }

  val toboggan = TobogganTrajectory("y2020/3.txt")

  println(s"Part 1: ${toboggan.sled(3,1).sum}")
  println

  val traversals: Array[(Int,Int)] = Array(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
  )

  val results: Array[Int] = traversals.map{ case (right: Int, down: Int) =>
    val out = toboggan.sled(right,down).sum
    println(s"(Right: $right, Down: $down): $out")
    out
  }

  // Long conversion is important because there will be integer overflow
  // Final answer is product of all answers for traversal methods in traversals
  println(s"Final answer: ${results.foldRight(1L){_ * _}}")
}

class TobogganTrajectory(val hill: Array[Array[Int]]) {
  val hillWidth: Int = hill(0).length

  /**
   * Case class makes it easier to pass data around
   * Note that x and y are in math coordinates, reverse of array coordinates
   *
   * @param x location along row
   * @param y location along column
   */
  case class Position(x: Int, y: Int) {
    def at: Int = hill(y)(x)
    def update(dx: Int, dy: Int): Position = Position((x + dx) % hillWidth, y + dy)
  }

  def sled(dx: Int, dy: Int): Iterator[Int] = Iterator.unfold((0, Position(0, 0))) {
    case (i: Int, pos: Position) if (pos.y < hill.length) => Some((pos.at, (pos.at, pos.update(dx, dy))))
    case _ => None
  }
}