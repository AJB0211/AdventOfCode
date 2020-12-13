package y2020.day4

import ajb0211.Advent.util.readFile

/**
 * I hated all of this and take no pride in this garbage solution
 */
object PassportProcessing extends App{
  val fieldNames: Array[String] = Array(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"
    //"cid"
  )

  val fieldMap: Map[String,Int] = fieldNames.zipWithIndex.toMap
  val pattern = raw"(\w{3})[:]".r

  // For part 1
  def extractLine(line: String): Iterator[String] = pattern.findAllMatchIn(line).map(_.group(1))

  // For part 2
  def parseLine(line: String): Map[String,String] = {
    line
      .split(' ')
      .map{_.split(':') match {
        case Array(a,b) => (a,b)
        case _ => null
      }}.toMap
  }

  // Part 1
  def validatePassports(doc: Iterator[String]): Int = {
    var passport: Int = 0
    var acc: Int = 0

    for (line <- doc){
      if (line.isEmpty) {
        acc += {if (passport == 127) 1 else 0}
        passport = 0
      } else {
        extractLine(line).foreach {
          fieldMap.get(_) match {
            case Some(i) => passport |= 1 << i
            case _ => None
          }
        }
      }
    }

    //Account for the last line in the file
    acc + {if (passport == 127) 1 else 0}
  }

  print("Part 1: ")
  println(
    validatePassports(
      readFile("y2020/4.txt")
    )
  )
  println

  def validPassport(map: collection.mutable.Map[String,String]): Boolean = {
    if (!fieldNames.forall(map.contains)) return false

    Passport(
      map("byr").toInt,
      map("iyr").toInt,
      map("eyr").toInt,
      map("hgt"),
      map("hcl"),
      map("ecl"),
      map("pid"),
      0
    ).isValid
  }

  def validatePassportFields(doc: Iterator[String]): Int = {
    var passport = collection.mutable.Map[String,String]()
    var acc: Int = 0

    for (line <- doc){
      if (line.isEmpty) {
        acc += {if (validPassport(passport)) 1 else 0}
        passport.clear
      } else {
        passport ++= parseLine(line)
      }
    }
    
    // Account for the last line in the file
    acc + {if (validPassport(passport)) 1 else 0}
  }

  print("Part 2: ")
  println(
    validatePassportFields(
      readFile("y2020/4.txt")
    )
  )

}

case class Passport(
    byr: Int,    // Birth Year
    iyr: Int,    // Issue Year
    eyr: Int,    // Expiration Year
    hgt: String, // Height
    hcl: String, // Hair Color
    ecl: String, // Eye Color
    pid: String, // Passport ID
    cid: Int     // Country ID
                  ) {
  def validByr: Boolean = byr <= 2002 && byr >= 1920

  def validIyr: Boolean = iyr <= 2020 && iyr >= 2010

  def validEyr: Boolean = eyr <= 2030 && eyr >= 2020

  def validHgt: Boolean =
    if (hgt.takeRight(2) == "in") {
      val hgtInt = hgt.dropRight(2).toInt
      hgtInt >= 59 && hgtInt <= 76
    } else if (hgt.takeRight(2) == "cm") {
      val hgtInt = hgt.dropRight(2).toInt
      hgtInt >= 150 && hgtInt <= 193
    } else false


  def validHcl: Boolean = {
    if (hcl(0) != '#') return false
    if (hcl.length != 7) return false
    for (c <- hcl.tail){
      /*
      0 -> 48
      9 -> 57
      a -> 92
      f -> 102
       */
      val cVal = c.toInt
      if ( cVal < 48 || (cVal > 57 && cVal < 97) || cVal > 102 ) return false
    }

    true
  }

  def validEcl: Boolean = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains ecl

  def validPid: Boolean = (pid forall Character.isDigit) && (pid.length == 9)

  def isValid: Boolean = validByr && validIyr && validEyr && validHgt && validHcl && validEcl && validPid
}