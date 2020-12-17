package y2020.day15

import ajb0211.Advent.util.readFile

import scala.collection.mutable

object RambunctiousRecitation extends App {
  val game = ElfGame("y2020/15.txt")

  println("Part 1: 2020th utterance")
  println(game.play(2020))

  println("Part 2: 30000000th utterance")
  println(game.play(30000000))

}