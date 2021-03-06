package y2020.day16

import ajb0211.Advent.util.readFile

import scala.collection.mutable

case class TicketField(name: String, min1: Int, max1: Int, min2: Int, max2: Int){
  def isFieldValid(i: Int): Boolean = ((i >= min1) && (i <= max1)) || ((i >= min2) && (i <= max2))
  def isNotFieldValid(i: Int): Boolean = !isFieldValid(i)

  lazy val toSet: Set[Int] = (min1 to max1).toSet union (min2 to max2).toSet
}

class Notes(val fields: Array[TicketField], val yourTicket: Array[Int], val tickets: Array[Array[Int]]) {
  // Set of valid values, used to find what values define invalid tickets
  lazy val validValues: Set[Int] = fields.foldLeft(Set.empty[Int]){ _ union _.toSet}

  lazy val invalidTicketValues: Array[Int] = tickets.flatMap( ticket => ticket.filterNot(validValues contains) )
  lazy val sumInvalidValues: Int = invalidTicketValues.sum

  def isValidTicket(ticket: Array[Int]): Boolean = ticket.forall(validValues contains)
  lazy val validTickets: Array[Int] = tickets.indices.filter{ i => isValidTicket(tickets(i))}.toArray

  /**
   * Constructs map from index in fields to index in each ticket
   */
  lazy val ticketFieldMap: Map[Int,Int] = {
    // Array of what each field can be
    val idxSetArray = Array.fill(fields.length)(mutable.Set.empty[Int])
    idxSetArray.foreach{_.addAll(fields.indices)}

    // for all valid tickets
    validTickets.foreach{ t =>
      // for all elements of a ticket
      fields.indices.foreach{ ti =>
        // For remaining possible values in each set
        idxSetArray(ti).foreach{ fj =>
          // If element cannot be in that set
          if (fields(fj).isNotFieldValid(tickets(t)(ti))) {
            // Remove it
            idxSetArray(ti) -= fj
        }
      }
    }}

    /*
      Now clean up the sets by taking disjunctions
      Unclear if this solution is general
      But it makes sense that if there aren't sufficient fields with known locations at this point
      The problem may not have a unique solution
     */
    while (idxSetArray.exists{(s: mutable.Set[Int]) => s.size > 1}) {
      for (set <- idxSetArray;
           set2 <- idxSetArray
      // Guard against same set because otherwise you'll wipe it
           if !(set eq set2)){
        if (set2.size == 1){
          set --= set2
        }
      }
    }

    idxSetArray.zipWithIndex.map{ case(s, i) => (s.head, i)}.toMap[Int,Int]
  }

  /**
   * Finds "departure" fields from ticket names
   */
  lazy val departureIndices: Array[Int] = {
    val departure = raw"^depart".r
    fields.indices.filterNot{ i =>
      departure.findFirstIn(fields(i).name).isEmpty
    }.toArray
  }

  // Solution to Part 2
  def yourDepartureValues: Array[Int] = departureIndices.map( i => yourTicket(ticketFieldMap(i)))
  def yourDepartureProduct: Long = yourDepartureValues.foldRight(1L)(_ * _)
}


object Notes {
  // Regexes for line parsing
  private val namePattern = raw"^([\s\w]+)".r
  private val numPattern = raw"(\d+)".r

  /**
   * Parser for lines associated with ticket fields
   * @param line input string
   * @return TicketField object describing field
   */
  def parseFieldLine(line: String): TicketField = {
    val name = namePattern.findFirstIn(line).get
    val rangeVals = numPattern.findAllIn(line)

    TicketField(name, rangeVals.next.toInt, rangeVals.next.toInt, rangeVals.next.toInt, rangeVals.next.toInt)
  }

  /**
   * Parser for line associated with tickets
   * @param line input string
   * @return array of field values
   */
  def parseTicket(line: String): Array[Int] = line.split(",").map(_.toInt)

  /**
   * Read notes and generate Notes class containing this information
   * @param file path in resources
   * @return new Notes object
   */
  def readInput(file: String): Notes = {
    val notes: Iterator[String] = readFile(file)

    val fields = notes.takeWhile(_ != "").map(parseFieldLine).toArray

    // skip blank space
    notes.next
    val yourTicket = parseTicket(notes.next)
    // skip blank space
    // skip "nearby tickets:"
    notes.drop(2)

    val tickets = notes.map(parseTicket).toArray

    new Notes(fields, yourTicket, tickets)
  }

  /**
   * Convenient object constructor
   */
  def apply(file: String): Notes = readInput(file)
}