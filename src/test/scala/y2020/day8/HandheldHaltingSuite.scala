package y2020.day8

import org.scalatest.funsuite.AnyFunSuite
import HandheldHalting._
import ajb0211.Advent.util.readFile

class HandheldHaltingSuite extends AnyFunSuite{
  type Instructions = Array[Instruction]

  val instructions: Instructions = readFile("y2020/8_test.txt").map{Instruction(_)}.toArray

  val cycleStart = untilLoop(instructions)
  val accBefore = State().runNtimes(instructions, cycleStart).acc
  val fixedSol = runAll(fixInstructions(instructions))

  test("Accumulator value before cycle should be 5") {assert(accBefore == 5)}

  test("Accumulator value after termination should be 8") {assert(fixedSol == 8)}
}
