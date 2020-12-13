package y2020.day9

import java.util.EmptyStackException

import ajb0211.Advent.util.readFile

object EncodingError extends App {
  def encoding: Iterator[Int] = readFile("y2020/9.txt").map(_.toInt)

  // If using test file, set to 5
  val preambleLength = 25

  val jfc = new XMAS(encoding, preambleLength)
  val part1 = jfc.firstInvalid
  println("Part 1")
  println(s"First invalid element: $part1")
  println()

  val part2 = jfc.sumRange(part1)
  println("Part 2")
  println(s"Sum of elements in run adding to invalid element: $part2")
}

/**
 * Solver for "XMAS" encoding from problem description
 * This is definitely over engineered due to planning for part 2... which turned out to be unsubstantial
 * Partially due to the fact that it is unclear if duplicates will be contained in the data set
 *
 * This solution does not scale well for finding the invalid key. Goes with the square of the preamble length
 * @param encoding input data
 * @param preambleLength number of previous elements to store, by problem definition
 */
class XMAS(val encoding: Iterator[Int], val preambleLength: Int = 25) {
  private var isFull: Boolean = false
  private val seen = collection.mutable.ArrayBuffer.empty[Int]
  private val prev: Array[Int] = Array.fill(preambleLength)(0)
  private var it: Int = 0

  // CONSTRUCTOR
  for (value <- encoding.take(preambleLength)){
    update(value)
  }

  /**
   * Helper to manage increment of iterator for prev object
   */
  private def increment(): Unit = {
    if (it == preambleLength - 1) {isFull = true}
    it = (it + 1) % preambleLength
  }

  private def next(): Integer = encoding.next()

  def isEmpty: Boolean = encoding.isEmpty
  def nonEmpty:Boolean = !isEmpty

  /**
   * Implements update rule over relevant data structures on observing a new value
   * Assumes validity check has already been performed
   * @param value value to add
   */
  def update(value: Int): Unit = {
    seen addOne prev(it)
    prev(it) = value
    increment()
  }

  /**
   * Checks if a value is valid for the encoding scheme
   * References active prev list for current run of elements
   * @param value value to check if valid
   * @return boolean if value is valid
   */
  def isValid(value: Int): Boolean = {
    if (!isFull) return true

    for (i <- prev.indices){
      for (j <- Range(i+1, prev.length)){
        if (prev(i) + prev(j) == value) return true
      }
    }

    false
  }

  /**
   * Finds first invalid value in the encoding list
   * @return first invalid value
   */
  def firstInvalid: Int = {
    while (nonEmpty) {
      val value = next()
      if (!isValid(value)) return value
      else update(value)
    }

    throw new EmptyStackException()
  }

  /**
   * Implements solution to part 2
   * Finds the range where there is a run of values adding up to target
   * Then takes the min and max values over that run and adds them
   *
   * @param target Result from firstInvalid
   * @return sum of values, solution to part 2
   */
  def sumRange(target: Int): Int = {
    // Inner function breaks the loop and returns the queue
    def buildQueue(): collection.mutable.Queue[Int] = {
      var acc = 0
      val q = collection.mutable.Queue.empty[Int]

      for (i <- seen){
        acc += i
        q.enqueue(i)

        // dequeue before checking if equal is important here
        // Otherwise, will miss case where dequeue produces the correct value
        // rather than just enqueue
        while (acc > target) acc -= q.dequeue()

        if (acc == target) return q
      }

      throw new Exception("Run not found")
    }

    /**
     * Case class aids the reduce operation on the queue
     * @param minVal minimum value in queue
     * @param maxVal maximum value in queue
     */
    case class ValuePair(minVal: Int = Integer.MAX_VALUE, maxVal:Int = Integer.MIN_VALUE) {
      def update(i: Int): ValuePair = ValuePair(math.min(minVal, i), math.max(maxVal, i))

      def sum: Int = maxVal + minVal
    }

    buildQueue().foldLeft(ValuePair()){ case (pair: ValuePair, i: Int) => pair.update(i)}.sum
  }

}