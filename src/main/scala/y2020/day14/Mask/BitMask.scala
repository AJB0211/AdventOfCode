package y2020.day14.Mask

class BitMask(line: String = " = ") extends Mask[String](line) {

  override def swap(a: Char, b: Char): Char = b match {
    case '1' => '1'
    case '0' => '0'
    case _ => a
  }

  override def applyMask(mem: String): String = mem.zip(mask).map { case (a, b) => swap(a, b) }.mkString
}