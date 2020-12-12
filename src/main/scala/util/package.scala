package ajb0211.Advent

package object util {
  def readFile(path: String): Iterator[String] = io.Source.fromResource(path).getLines
}
