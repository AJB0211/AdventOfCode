package y2020.day15

import ajb0211.Advent.util.readFile

import scala.collection.mutable

object RambunctiousRecitation extends App {
  def init: List[Int] = readFile("y2020/15.txt").next.split(",").map(_.toInt).toList

  val game = new ElfGame(init)

  println("Part 1: 2020th utterance")
  println(game.play(2020, false))

  println("Part 2: 30000000th utterance")
  println(game.play(30000000))

}

class ElfGame {
  protected val map: mutable.Map[Int, Int] = collection.mutable.Map[Int,Int]()
  var turn: Int = 1
  var last: Int = 0

  def this(init: List[Int]) = {
    this
    init.foreach(update)
  }

  protected def update(i: Int): Int = {
    map.update(last, turn)
    turn += 1
    last = i
    i
  }

  protected def think: Int = map.get(last) match {
    case Some(t) => turn - t
    case _ => 0
  }


  def speak(loudly: Boolean = false): Int = {
    val x = think
    update(x)
    if (loudly) println(x)
    x
  }

  @annotation.tailrec
  final def play(lastTurn: Int = 2020, loudly: Boolean = false): Int =
    if (turn >= lastTurn) speak()
    else {speak(loudly); play(lastTurn, loudly)}

}