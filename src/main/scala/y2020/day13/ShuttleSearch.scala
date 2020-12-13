package y2020.day13

import ajb0211.Advent.util.readFile

/**
 * All inputs are prime, even if it isn't in the problem statement
 * see Chinese Remainder Theorem (CRT) for part 2
 */

object ShuttleSearch extends App {
  val schedule = Schedule("y2020/13.txt")

  println("Part 1: Earliest bus")
  println(schedule.earliest)
  println
  println("Part 2: Earliest time that defined sequence is true")
  println(schedule.minSeries)


}




class Schedule(val minTime:Int, val schedule: Array[Option[Int]]) {
  // Unclear why this isn't working
  //type Bus = Option[Int]

  /**
   * Convenience constructor to ait apply method in class object
   */
  def this(tuple: (Int, Array[Option[Int]])) = this(tuple._1,tuple._2)


  /**
   * Finds the earliest arrival time that is valid when "you" arrive at minTime
   * @return product of bus ID and your wait time, as described by solution input
   */
  def earliest: Int = {
    val minBus = schedule.foldLeft((0, Integer.MAX_VALUE)){ case ((idx, wait), bus) => bus match {
      case Some(t) =>
        val dt = t - minTime % t
        if (dt < wait) (t, dt) else (idx,wait)
      case _ => (idx, wait)
    }}

    minBus._1 * minBus._2
  }

  /**
   * Solves the defined problem in part 2 using the Chinese Remainder Theorem
   *  For a set of integers {n} which are all greater than one and pairwise coprime
   *  N := product({n})
   *  There are a set of integers {a}: a_i \in [0,n_i)
   *    where x % n_i = a_i
   *  Then there is only one unique integer x \in [0,N) that satisfies this relationship for every {(n_i,a_i)}
   *
   * Here, {n} are bus arrival times and {a} are their indices in the sequence provided by the input
   * We seek x
   *
   * This is a "Search by Sieve" solution
   * @return minimum time where series definition holds
   */
  def minSeries: Long = {
    // See comment for why these variables are names as such
    var a = 0     // index in list
    var x = 0L    // accumulator of checked results, will be solution
    var step = 1L // step size

    schedule.foreach {
      case Some(n) =>
        while ((x+a) % n != 0) {
          x += step
        }
        /*
         Step size here is the product seen buses
         This is due to the fact that all buses have prime time intervals, implying the set is coprime
         We are taking steps of the Least Common Multiple of the values we have seen so far
         These are the cases where the desired relationship will be maintained
         Notice that the relationship for some subset of values must hold for the whole set
         So we can build up to the solution by arithmetic progression
         The primeness of all numbers implies the LCM is the product of all values

         Alternately, in terms of CRT, see "Search by Sieving"
         */
        step *= n
        a += 1
      case _ => a += 1
    }

    x
  }

}

object Schedule {
  def readSchedule(file: String): (Int, Array[Option[Int]]) = {
    val in = readFile(file)
    val minTime = in.next.toInt
    val buses = in.next.split(",").map{ elem =>
      if (elem.forall(_.isDigit)) Some(elem.toInt)
      else None
    }
    (minTime, buses)
  }

  def apply(file:String) = new Schedule(readSchedule(file))
}