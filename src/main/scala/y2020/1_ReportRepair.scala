package ajb0211.Advent.y2020

import ajb0211.Advent.util.readFile

object ReportRepair extends App {
  val report: List[Int] = readFile("y2020/1.txt").toList.map(_.toInt)

  @annotation.tailrec
  def findComplementProduct(target: Int = 2020, report: List[Int], acc: Set[Int] = Set[Int]()): Unit = {
    if (acc contains (target - report.head)){
      println(report.head * (target-report.head))
    } else {
      findComplementProduct(target, report.tail, acc + report.head)
    }
  }

  // not sure why this isn't tail recursive
  def findTripleProduct(target: Int = 2020, report: List[Int], acc: Set[Int] = Set[Int]()): Unit = {
    acc.foreach{ (i: Int) =>
      if (acc contains (target - report.head - i)){
        println(i*report.head*(target-i-report.head))
        return
      }
    }
    findTripleProduct(target, report.tail, acc + report.head)
  }

  findComplementProduct(2020, report)
  findTripleProduct(2020, report)

}