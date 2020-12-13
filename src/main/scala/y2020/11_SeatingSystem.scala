package ajb0211.Advent.y2020

import ajb0211.Advent.util.readFile

object SeatingSystem extends App {
  val ferry = new Ferry("y2020/11.txt")

  ferry.relax
  val part1 = ferry.numOccupied
  println("Part 1: Number occupied after relaxation")
  println(part1)

  val ferry2 = new Ferry("y2020/11.txt")

  ferry2.relax2
  val part2 = ferry2.numOccupied
  println("Part 2: Number occupied after modified relaxation")
  println(part2)

}

class Ferry(var seating: Array[Array[Char]]){
  def height: Int = seating.length
  def width: Int = seating(0).length

  private val seatVal: Map[Char, Int] = Map(
    'L' -> 0,
    '.' -> 0,
    '#' -> 1
  )

  /**
   * Seat object implements logic for searching around an individual seat
   * Note that seat status is still stored in the outer array inside class Ferry
   * By nesting, this class has access to the array without having to worry about passing it
   * @param row
   * @param col
   */
  class Seat(val row: Int, val col: Int){

    def isValid: Boolean = (row >= 0) && (col >= 0) && (row < height) && (col < width)
    def notValid: Boolean = !isValid

    def value: Char = seating(row)(col)

    def isFloor: Boolean = value == '.'
    def notFloor:Boolean = !isFloor

    // occupation number
    def occNum: Int = seatVal(value)

    protected def getAdjacent: IndexedSeq[Seat] =
      for (i <- -1 to 1;
           j <- -1 to 1;
           seat = new Seat(row + i, col + j)
           // Make sure this seat is not being returned
           if !((i==0) && (j==0)) && seat.isValid) yield seat

    def numAdjacent: Int = getAdjacent.map(_.occNum).sum

    def swap: Char =
      if      (seating(row)(col) == 'L') '#'
      else if (seating(row)(col) == '#') 'L'
      else throw new Exception(s"Cannot swap floor at ($row,$col)")

    def isSwap: Boolean =
      if      ((value == 'L') && (numAdjacent == 0)) true
      else if ((value == '#') && (numAdjacent >= 4)) true
      else false

  }

  /**
   * Implements new adjacency logic for Part 2
   * @param row
   * @param col
   */
  private class FarSeat(row: Int, col: Int) extends Seat(row, col) {
    protected def firstInDirection(di: Int, dj: Int): Option[FarSeat] = {
      var i = di
      var j = dj

      while (true){
        val seat = new FarSeat(row + i, col + j)
        if (seat.notValid) return None
        else if (seat.notFloor) return Some(seat)
        else {i += di; j += dj}
      }

      // for type check
      None
    }

    override protected def getAdjacent: IndexedSeq[FarSeat] = {
      for (di <- -1 to 1;
           dj <- -1 to 1;
           if !((di == 0) && (dj == 0))) yield firstInDirection(di, dj)
    }.flatten

    override def isSwap: Boolean =
      if      ((value == 'L') && (numAdjacent == 0)) true
      else if ((value == '#') && (numAdjacent >= 5)) true
      else false
  }


  def this(seatFile: String) = this(readFile(seatFile).map(_.toArray).toArray)

  override def toString: String = {
    seating.map(_.mkString(" ")).mkString("\n")
  }

  def iterate[T <: Seat](constructor: (Int,Int)=>T): Boolean = {
    var swapFlag = false
    val tempSeating = seating.map(_.clone)

    for (i <- seating.indices;
         j <- seating(i).indices){
      val seat = constructor(i,j)
      if (seat.isSwap) {swapFlag = true; tempSeating(i)(j) = seat.swap}
    }

    seating = tempSeating
    swapFlag
  }

  def relax: Unit  = while(iterate(new Seat(_,_))){}
  def relax2: Unit = while(iterate(new FarSeat(_,_))){}

  def numOccupied: Int = {
    for (i <- seating.indices;
         c <- seating(i)) yield seatVal(c)
  }.sum

}