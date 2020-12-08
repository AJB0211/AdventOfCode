package ajb0211.Advent.y2020

import collection.mutable.{ArrayBuffer}

object HandyHaversacks extends App {
  def readFile(path: String): Iterator[String] = io.Source.fromResource(path).getLines
  def bags = readFile("y2020/7.txt")

  val graph = new SackGraph(bags)

  println("Part 1:\nNumber of parents of shiny gold")
  println(graph.countParents("shiny gold"))
  println
  println
  println("Part 2:\nNumber of bags in shiny gold")
  println(graph.countChildren("shiny gold"))
}

case class Child(index: Int, number: Int)

case class Bag(var color: String, val index: Int, var visited: Boolean = false){
  val parents: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  val children: ArrayBuffer[Child] = ArrayBuffer.empty[Child]

  def visit: Unit = {visited = true}
}

class SackGraph[T]{
  val sackMap: collection.mutable.Map[String,Int] = collection.mutable.Map[String,Int]()
  val nodes: ArrayBuffer[Bag] = ArrayBuffer.empty[Bag]

  def this(bagInput: Iterator[String]) = {
    this()
    bagInput foreach addNodeFromLine
  }

  private def parseLine(line: String): (String, Iterator[(Int,String)]) = {
    val pattern = raw"(\d+)\s([\w\s]+)\sbags?".r

    val Array(parent: String, children: String) = line.split(raw"\scontain\s")

    // In the case of an empty bag, "no other bags" will not produce any matches leaving an empty iterator
    val childPairs =  pattern.findAllMatchIn(children).map( regMatch => (regMatch.group(1).toInt, regMatch.group(2)))
    (parent.dropRight(5), childPairs)
    }

  /**
   * Returns index of existing node
   * Or adds new node to graph and sackMap then returns new node index
   * @param key name of bag color
   * @return integer index of bag in Array nodes
   */
  def getOrAddNodeIndex(key: String): Int = sackMap.getOrElseUpdate(key, {
    nodes.addOne(Bag(key, sackMap.size))
    sackMap.size
  })

  /**
   * Adds a new node from the line parsed by parseLine
   * updating sackMap and nodes in the process
   * @param line
   */
  def addNode(line: (String, Iterator[(Int, String)])): Unit = {
    // Get existing key from map
    // Else, add node to graph and return new node index
    val parentKey: Int = getOrAddNodeIndex(line._1)

    line._2.foreach{ case (num: Int, color: String) =>
      val childKey = getOrAddNodeIndex(color)
      nodes(childKey).parents addOne parentKey
      nodes(parentKey).children addOne Child(index = childKey, number = num)
    }
  }

  def addNodeFromLine(line: String): Unit = addNode(parseLine(line))

  def countParents(idx: Int): Int = {
    var stack: List[Int] = nodes(idx).parents.toList
    nodes(idx).visit
    var acc: Int = 0

    while (stack.nonEmpty){
      val node = nodes(stack.head)
      if (!node.visited){
        node.visit
        acc += 1
        stack = node.parents.toList ::: stack.tail
      } else {stack = stack.tail}
    }

    acc
  }

  def countParents(color: String): Int = countParents(sackMap(color))

  def countChildren(idx: Int): Long = nodes(idx).children.map{ (bag: Child) =>
      bag.number + bag.number*countChildren(bag.index)
  }.sum

  def countChildren(color: String): Long = countChildren(sackMap(color))


  def resetVisits: Unit = nodes foreach {_.visited = false}

}