package y2020.day14

import ajb0211.Advent.util.readFile
import y2020.day14.Register.{Version1, Version2}

import scala.collection.mutable

object DockingData extends App {
  def input = readFile("y2020/14.txt")

  val v1 = Version1.run(input)
  val v2 = Version2.run(input)

  println("Part 1: Version 1 Bitmask")
  println(v1.readSum)
  println
  println("Part 2: Version 2 Address Decoder")
  println(v2.readSum)
}





