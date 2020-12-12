package ajb0211.Advent.y2020

import ajb0211.Advent.util.readFile

object CustomCustoms extends App {
  def papers: Iterator[String] = readFile("y2020/6.txt")

  def countAny(doc: Iterator[String]): Int = {
    val set = collection.mutable.HashSet[Char]()
    var acc: Int = 0

    for (line <- doc){
      if (line.isEmpty){
        acc += set.size
        set.clear
      } else {
        set ++= line
      }
    }

    acc + set.size
  }

  def countAll(doc: Iterator[String]): Int = {
    var acc: Int = 0
    var setList: List[Set[Char]] = List.empty[Set[Char]]

    for (line <- doc){
      if (line.isEmpty){
        acc += setList.reduce(_ intersect _).size
        setList = List.empty[Set[Char]]
      }
      else {
        setList = line.toSet :: setList
      }
    }

    if (setList.isEmpty) acc
    else acc + setList.reduce(_ intersect _).size
  }

  println(s"Part 1:\n\t${countAny(papers)}")
  println(s"Part 2:\n\t${countAll(papers)}")
}