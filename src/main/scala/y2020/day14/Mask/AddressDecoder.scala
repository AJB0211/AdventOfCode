package y2020.day14.Mask

class AddressDecoder(line: String = " = ") extends Mask[List[String]](line) {
  override def swap(a: Char, b: Char): Char = b match {
    case '1' => '1'
    case '0' => a
    case _ => 'X'
  }

  private def appendLists(vals: List[Char], lsts: List[List[Char]]): List[List[Char]] =
    vals
      .flatMap(c =>
        lsts
          .map(acc => c :: acc)
      )

  override def applyMask(mem: String): List[String] =
    mem.toList.zip(mask.toList).foldRight(List(List.empty[Char])){
      case ((a,b), acc) => swap(a,b) match {
        case 'X' => appendLists(List('0','1'), acc)
        case '1' => acc.map( '1' :: _)
        case '0' => acc.map( '0' :: _)
        case _ => throw new Exception()
      }
    }.map(_.mkString)


}