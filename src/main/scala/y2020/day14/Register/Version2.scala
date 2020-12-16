package y2020.day14.Register

import y2020.day14.Mask.AddressDecoder

import scala.collection.mutable

class Version2 extends Register[Long, Int, List[String], AddressDecoder] {
  override val register: mutable.Map[Long, Int] = mutable.Map.empty[Long,Int]
  override val maskConstructor: String => AddressDecoder = new AddressDecoder(_)
  override var mask: AddressDecoder = new AddressDecoder()

  /**
   * Using manual sum with long conversion is necessary because using sum will treat as int
   * Then convert to long at the end of the function call
   * @return Sum of values stored in all memory locations
   */
  override def readSum: Long = register.values.foldRight(0L)( _ + _ )

  override def parseMemLine(line: String): Unit = {
    val (mem, value) = extractLine(line)
    val maskedAddresses: List[String] = mask(intToBinRep(mem))

    maskedAddresses.foreach{ s => update(memToLong(s), value.toInt)}
  }
}

object Version2 {
  def run(file: Iterator[String]): Version2 = {
    val register = new Version2()
    file.foreach{ register.parseLine }

    register
  }
}