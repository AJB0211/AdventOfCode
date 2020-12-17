package y2020.day17

object ConwayCubes extends App {

  println("Part 1: Game of Life in 3D")
  println(Life("y2020/17.txt").run(6).numActive)
  println
  println("Part 2: Game of Life in 4D")
  println(HyperLife("y2020/17.txt").run(6).numActive)
}