package ajb0211.Advent.y2020

import ajb0211.Advent.util.readFile

object PasswordPhilosphy extends App {
  val passBank: List[Password] = readFile("y2020/2.txt").toList.map(Password(_))

  println(passBank.map((p:Password) => if (p.isValid) 1 else 0).sum)
  println(passBank.map((p:Password) => if (p.isValid2) 1 else 0).sum)
}

class Password(val password: String, val c: Char, val lower: Int, val upper: Int) {
  def isValid: Boolean = this.password.filter(_ == this.c).length match {
      case x if (this.lower <= x && x <= this.upper) => true
      case _ => false
  }

  def isValid2: Boolean = this.password(this.lower - 1) == c ^ this.password(this.upper - 1) == c

}

object Password {
  def apply(input: String): Password = {
    /*
    <lower>-<upper> <c>: <password>
     */
    val split: Array[String] = input.split(raw"\W+")

    new Password(split(3), split(2)(0), split(0).toInt, split(1).toInt)
  }
}