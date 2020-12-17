package y2020.day17

import ajb0211.Advent.util.readFile

class Life(val board: Board) {
  def update(cube: Cube, count: Int): Option[Cube] = if (board contains cube) {
    if ((count == 2) || (count == 3)) Some(cube)
    else None
  } else {
    if (count == 3) Some(cube)
    else None
  }

  private def buildAdjacencyMap = board.toSeq.
    flatMap(_.getAdjacent).
    groupBy(identity).
    view.mapValues(_.size)

  private lazy val nextBoard: Board = buildAdjacencyMap.
    flatMap{ case (cube, count) => update(cube, count)}.
    toSet

  def increment: Life = new Life(nextBoard)

  lazy val numActive: Int = board.size

  @annotation.tailrec
  final def run(n: Int): Life =
    if (n <= 0) this
    else increment.run(n-1)

}

object Life {
  def fromFile(path: String): Life = new Life (
    readFile(path).zipWithIndex.flatMap{ case (line, y) =>
      line.zipWithIndex.flatMap{ _ match {
        case ('#', x) => Some(Cube(x,y,0))
        case _ => None
      }}
    }.toSet
  )

  def apply(path: String): Life = fromFile(path)

}