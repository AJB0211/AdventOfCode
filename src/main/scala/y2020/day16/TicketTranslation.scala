package y2020.day16

object TicketTranslation extends App {

  val notes = Notes("y2020/16.txt")

  /*
  println("tickets")
  notes.fields.foreach(println)
  println("\nyour ticket")
  println(notes.yourTicket.mkString(","))
  println("\nother tickets")
  notes.tickets.foreach{ s=> println(s.mkString(","))}
  */

  println("Part 1: Sum of Invalid Ticket Values")
  println(notes.sumInvalidValues)
  println
  println("Part 2: Product of \"departure\" fields")
  println(notes.yourDepartureProduct)



}




