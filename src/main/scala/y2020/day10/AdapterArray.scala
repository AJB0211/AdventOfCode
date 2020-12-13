package y2020.day10

import ajb0211.Advent.util.readFile

object AdapterArray extends App {
  def adapters: Array[Int] = readFile("y2020/10.txt").map(_.toInt).toArray

  def joltDifferences(adapters: Array[Int]): Int = {
    var ones = 0
    var threes = 1 // to account for your device

    val sorted = adapters.sorted
    var last = 0

    sorted.foreach{ (i: Int) => i - last match {
      case x if x == 1 => {
        ones += 1
        last = i
      }
      case x if x == 2 => {
        last = i
      }
      case x if x == 3 => {
        threes += 1
        last = i
      }
      case _ => throw new Exception("Cannot stack these adapters")
    }
    }

    ones * threes
  }

  def joltCombinations(adapters: Array[Int]): Long = {
    val sorted = adapters.sorted
    val dynamically = Array.fill(sorted.last+1)(0L)
    dynamically(0) = 1

    sorted.foreach { (i: Int) =>
      if      (i == 1) dynamically(i) = dynamically(i-1)
      else if (i == 2) dynamically(i) = dynamically(i-1) + dynamically(i-2)
      else             dynamically(i) = dynamically(i-1) + dynamically(i-2) + dynamically(i-3)
    }

    dynamically.last
  }

  println("Part 1: Product of deltas 1 and 3")
  println(joltDifferences(adapters))
  println
  println("Part 2: Number of different ways to combine adapters")
  println(joltCombinations(adapters))
}
