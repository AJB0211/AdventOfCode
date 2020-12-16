package y2020.day14.Register

import y2020.day14.Mask.Mask

import scala.collection.mutable
import scala.util.matching.Regex

// This type signature is stupid but a good exercise in polymorphism
abstract class Register[K, V, I, T <: Mask[I]] {
  val register: mutable.Map[K,V]
  // Define the constructor as a lambda, may be some better way to do this but unclear
  val maskConstructor: String => T
  // Will use BitMask for part 1
  //  Address Decoder for part 2
  var mask: T
  final protected val inputPattern: Regex = raw"mem\[(\d+)\] = (\d+)".r

  def update(idx: K, value: V): Unit = register.update(idx, value)

  /**
   * Sum over all values stored at each memory address
   * @return solution
   */
  def readSum: Long

  /**
   * Integer to Binary Representation
   * @param s input from parsed line, still in string form but an integer
   * @return padded binary representation as a string
   */
  final protected def intToBinRep(s: String): String = {
    val sig = s.toInt.toBinaryString
    "0"*(mask.length - sig.length) + sig
  }

  /**
   * Converts a memory string to a long
   * @param s binary representation of integer
   * @return base10, as long
   */
  final protected def memToLong(s: String): Long = s.dropWhile(_ == '0').
    foldLeft(0L){case (acc, c) => 2*acc + c.asDigit}

  final protected def parseLine(line: String): Unit = {
    if (line.take(4) == "mask") mask = maskConstructor(line)
    else parseMemLine(line)
  }

  final protected def extractLine(line: String): (String, String) = line match {
    case inputPattern(idx, value) => (idx, value)
    case _ => throw new Exception("Bad Line Read")
  }

  /**
   * Define action each class will take on receiving a memory update
   * Derived class will either have to update a single memory location with derived value
   * Or all memory derived locations with explicit value
   *
   * Will need to call extractlLine
   * @param line Input line from file
   */
  def parseMemLine(line: String): Unit


}


