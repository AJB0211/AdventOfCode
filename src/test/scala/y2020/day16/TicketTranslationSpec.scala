package y2020.day16

import org.scalatest.flatspec.AnyFlatSpec

class TicketTranslationSpec extends AnyFlatSpec {
   "Part 1 test" should "find error scanning rate of 71" in {
     assert(Notes("y2020/16_test.txt").sumInvalidValues == 71)
   }

  "Part 2 test" should "find correct ticket field mappings" in {
    val fieldMap = Notes("y2020/16_test.txt").ticketFieldMap.toSet
    assert(fieldMap contains (0,1))
    assert(fieldMap contains (1,0))
    assert(fieldMap contains (2,2))
  }

}
