package y2020

/**
 * It would be smart to define a general cube in n-dim
 * Then make Cube and HyperCube instances of 4-dim
 * ... but that's a lot of work
 * Instead we just copy our code and slap "Hyper" in front of everything
 */
package object day17 {
  type Board = Set[Cube]
  case class Cube(x: Int, y: Int, z: Int){
    def getAdjacent = for (
      xi <- -1 to 1;
      yi <- -1 to 1;
      zi <- -1 to 1
      if (xi != 0) || (yi != 0) || (zi != 0)
    ) yield Cube(x+xi, y+yi, z+zi)
  }

  case class HyperCube(x: Int, y: Int, z: Int, t: Int){
    def getAdjacent = for (
      xi <- -1 to 1;
      yi <- -1 to 1;
      zi <- -1 to 1;
      ti <- -1 to 1
      if (xi != 0) || (yi != 0) || (zi != 0) || (ti != 0)
    ) yield HyperCube(x+xi, y+yi, z+zi, t+ti)
  }
  type HyperBoard = Set[HyperCube]

}
