package y2020.day17

import ajb0211.Advent.util.readFile

import scala.collection.MapView

class HyperLife(val board: HyperBoard) {
  protected def update(cube: HyperCube, count: Int): Option[HyperCube] = if (board contains cube) {
    if ((count == 2) || (count == 3)) Some(cube)
    else None
  } else {
    if (count == 3) Some(cube)
    else None
  }

  protected def buildAdjacencyMap: MapView[HyperCube, Int] = board.toSeq.
    flatMap(_.getAdjacent).
    groupBy(identity).
    view.mapValues(_.size)

  protected lazy val nextBoard: HyperBoard = buildAdjacencyMap.
    flatMap{ case (cube, count) => update(cube, count)}.
    toSet

  def increment: HyperLife = new HyperLife(nextBoard)

  lazy val numActive: Int = board.size

  @annotation.tailrec
  final def run(n: Int): HyperLife =
    if (n <= 0) this
    else increment.run(n-1)

}

object HyperLife {
  def fromFile(path: String): HyperLife = new HyperLife (
    readFile(path).zipWithIndex.flatMap{ case (line, y) =>
      line.zipWithIndex.flatMap{ _ match {
        case ('#', x) => Some(HyperCube(x,y,0,0))
        case _ => None
      }}
    }.toSet
  )

  def apply(path: String): HyperLife = fromFile(path)

}