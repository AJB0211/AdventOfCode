package y2020.day14.Register

import y2020.day14.Mask.BitMask

import scala.collection.mutable

class Version1 extends Register[Int, String, String, BitMask] {
  val maskConstructor = new BitMask(_)
  var mask: BitMask = new BitMask()
  override val register: mutable.Map[Int, String] = mutable.Map.empty[Int, String]

  override def readSum: Long = register.values.map(memToLong).sum

  override def parseMemLine(line: String): Unit = {
    val (idx, mem) = extractLine(line)
    val masked: String = mask(intToBinRep(mem))

    update(idx.toInt, masked)
  }
}

object Version1 {
  def run(file: Iterator[String]): Version1 = {
    val register = new Version1()
    file.foreach{ register.parseLine }

    register
  }
}