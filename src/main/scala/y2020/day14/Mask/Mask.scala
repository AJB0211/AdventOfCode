package y2020.day14.Mask

abstract class Mask[T](line: String = " = ") {
  // Automatically parses input line to return mask from the line
  val getLine: String => String = { line => line.split("=")(1).trim }
  val mask: String = getLine(line)
  lazy val length: Int = mask.length

  def swap(a: Char, b: Char): Char
  def applyMask(mem: String): T

  def apply(mem: String): T = applyMask(mem)
}

