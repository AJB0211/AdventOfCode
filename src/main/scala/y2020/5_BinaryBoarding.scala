package ajb0211.Advent.y2020

import java.util.InputMismatchException

object BinaryBoarding extends App {
  val docs: Iterator[String] = io.Source.fromResource("y2020/5.txt").getLines

  def parseSegment(pass: String, startIdx: Int, endIdx: Int, lowChar: Char): Int = {
    var lower = 0
    var upper = (1 << (endIdx - startIdx)) - 1

    for (i <- startIdx until endIdx)
      if (pass(i) == lowChar) {upper -= ((upper - lower) / 2) + 1}
      else {lower += ((upper - lower) / 2) + 1}

    if (pass(endIdx - 1) == lowChar)  lower
    else upper
  }

  def rowParse(pass: String): Int = parseSegment(pass, 0, 7, 'F')
  def colParse(pass: String): Int = parseSegment(pass, 7, 10, 'L')

  def seatIdx(pass: String): Int = rowParse(pass)*8 + colParse(pass)

  val seatHeap = collection.mutable.PriorityQueue[Int]()

  // Solution to part 1 also builds queue for part 2
  val part1 = docs.foldRight(0){ case (pass: String, acc: Int) => {
      val currIdx = seatIdx(pass)
      seatHeap.enqueue(currIdx)
      math.max(currIdx, acc)
    }}

  println(s"Part 1:\nHighest seat index: $part1\n")

  /**
   * pop values off heap to find where gap is 2
   * Answer is between those two values
   * This is a max heap so we read from the highest seat index going down
   * @param heap
   * @return your seat index, an integer
   */
  def readHeap(heap: collection.mutable.PriorityQueue[Int]): Int = {
    // Because this is a max heap
    var last = Integer.MAX_VALUE
    while (heap.nonEmpty){
      val curr = heap.dequeue
      if ((last-curr) == 2){
        return last - 1
      }
      last = curr
    }

    throw new InputMismatchException("No valid seat found")
    Integer.MIN_VALUE
  }
  println(s"Part 2:\nYour seat index: ${readHeap(seatHeap)}")

}