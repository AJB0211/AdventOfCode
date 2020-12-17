package y2020.day16

object TicketTranslation extends App {

  val notes = Notes("y2020/16.txt")

  println("Part 1: Sum of Invalid Ticket Values")
  println(notes.sumInvalidValues)
  println
  println("Part 2: Product of \"departure\" fields")
  println(notes.yourDepartureProduct)

}




