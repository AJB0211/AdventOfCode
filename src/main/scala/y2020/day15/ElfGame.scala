package y2020.day15

import ajb0211.Advent.util.readFile

import scala.collection.mutable

class ElfGame {
  protected val map: mutable.Map[Int, Int] = collection.mutable.Map[Int,Int]()
  var turn: Int = 1
  var last: Int = 0

  // Take initial list of turns to populate class
  def this(init: List[Int]) = {
    this
    init.foreach(update)
  }

  /**
   * Implements update logic for each turn
   * @param i Next value to store
   */
  protected def update(i: Int): Unit = {
    map.update(last, turn)
    turn += 1
    last = i
  }

  /**
   * Gets value for this turn
   * @return what this turn's elf will say
   */
  protected def think: Int = map.get(last) match {
    case Some(t) => turn - t
    case _ => 0
  }


  /**
   * Elf takes a turn
   * @param loudly if value should be printed to console
   * @return value for current turn
   */
  def speak(loudly: Boolean = false): Int = {
    val x = think
    update(x)
    if (loudly) println(x)
    x
  }

  /**
   * Play game until turn is reached
   * If a lastTurn is specified greater than this.turn, incorrect behavior
   *    next turn will be taken and return immediately
   *    not worth fixing for vanity
   *
   * @param lastTurn last turn Elves will play
   * @param loudly if every turn will print to console
   * @return last value spoken by elves
   */
  @annotation.tailrec
  final def play(lastTurn: Int = 2020, loudly: Boolean = false): Int =
    if (turn >= lastTurn) speak()
    else {speak(loudly); play(lastTurn, loudly)}

}

object ElfGame{
  def fromFile(path: String): ElfGame = new ElfGame(readFile(path).next.split(",").map(_.toInt).toList)

  def apply(path: String): ElfGame = fromFile(path)
}