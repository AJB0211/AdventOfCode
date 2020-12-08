package ajb0211.Advent.y2020

import java.util.InputMismatchException

object HandheldHalting extends App {
  type Instructions = Array[Instruction]

  def readFile(path: String): Iterator[String] = io.Source.fromResource(path).getLines
  val instructions: Instructions = readFile("y2020/8.txt").map{Instruction(_)}.toArray

  /**
   * Detects if a cycle is present
   * @param instructions
   * @return boolean is cycle?
   */
  def isCycle(instructions: Instructions, start: Int = 0): Boolean = {
    var slow: State = State(0, start)
    var fast: State = State(0, start)

    while ((fast.it < instructions.length) && (fast.run(instructions).it < instructions.length)) {
      slow = slow.run(instructions)
      fast = fast.run(instructions).run(instructions)

      if (slow.it == fast.it){
        return true
      }
    }

    false
  }

  def notCycle(instructions: Instructions, start: Int = 0): Boolean = !isCycle(instructions, start)

  /**
   * Finds the index where the cycle begins
   * This solution assumes a cycle is present in the data
   * If not cycle, returns -1
   * @param instructions
   * @return index of cycle start in instructions
   */
  def indexOfCycle(instructions: Instructions): Int = {
    var slow: State = State()
    var fast: State = State()

    do {
      if ((fast.it == instructions.length) || (fast.run(instructions).it == instructions.length)){
        return -1 // no loop
      }
      slow = slow.run(instructions)
      fast = fast.run(instructions).run(instructions)
    } while (slow.it != fast.it)

    slow = State()

    while (slow.it != fast.it){
      slow = slow.run(instructions)
      fast = fast.run(instructions)
    }

    fast.it
  }

  /**
   * Finds the number of operations before an operation is returned to
   * @param instructions
   * @return number of operations before
   */
  def untilLoop(instructions: Instructions): Int = {
    @annotation.tailrec
    def inner(state: State = State(), seen: Set[Int] = Set.empty[Int], iters: Int = 0): Int = {
      if (seen contains state.it) iters
      else inner(state.run(instructions), seen + state.it, iters+1)
    }

    inner()
  }

  /**
   * Swaps instructions as defined in part 2 to find a correction to the instruction set
   * @param instructions
   * @return corrected instruction set
   */
  def fixInstructions(instructions: Instructions): Instructions = {
    val cycleStart = untilLoop(instructions)

    @annotation.tailrec
    def findCycleSet(currentState: State = State(0, cycleStart), set: Set[Int] = Set.empty[Int]): Set[Int] = {
      if (set contains currentState.it) set
      else findCycleSet(currentState.run(instructions), set + currentState.it)
    }

    val cycleIdx = findCycleSet().toArray
    for (idx <- cycleIdx){
      instructions(idx) match {
        case Nop(v) => {
          val temp = instructions.clone
          temp.update(idx, Jmp(v))
          if (notCycle(temp, cycleStart)) {return temp}
        }
        case Jmp(v) => {
          val temp = instructions.clone()
          temp.update(idx, Nop(v))
          if (notCycle(temp, cycleStart)) {return temp}
        }
        case _ => None
      }
    }

    throw new Exception("Ur code is fukt lol")
  }

  /**
   * Run program until termination
   * DO NOT USE THIS ON A CYCLE
   * @param instructions
   * @return the value of the accumulator on completion
   */
  def runAll(instructions: Instructions): Int = {
    @annotation.tailrec
    def inner(state: State): State = {
      if (state.it >= instructions.length) state
      else inner(state.run(instructions))
    }

    inner(State()).acc
  }

  val cycleStart = untilLoop(instructions)
  val accBefore = State().runNtimes(instructions, cycleStart).acc
  println("PART 1")
  println(s"Number of operations until repeat: $cycleStart")
  println(s"Value of accumulator at repeat: ${accBefore}")

  println

  val fixedSol = runAll(fixInstructions(instructions))
  println("PART 2")
  println("Value of accumulator after correction: $fixedSol")

}

/**
 * For state of program
 * Used to run operations incrementally and store update information
 * @param acc accumulator, see problem definition
 * @param it index of next line to run, see type Instructions
 */
case class State(acc: Int = 0, it: Int = 0){
  def act(instruction: Instruction): State = instruction match {
    case Nop(_) => State(acc, it+1)
    case Acc(i) => State(acc+i, it+1)
    case Jmp(i) => State(acc, it+i)
  }

  def run(instructions: Array[Instruction]): State = act(instructions(it))

  def runNtimes(instructions: Array[Instruction], n: Int): State = {
    if (n <= 0) this
    else this.run(instructions).runNtimes(instructions, n-1)
  }

}

/**
 * Abstraction for handling different operations
 * Allows for case matching
 */
sealed abstract class Instruction
case class Nop(inc: Int) extends Instruction
case class Acc(inc: Int) extends Instruction
case class Jmp(inc: Int) extends Instruction

/**
 * To generate a constructor for Instructions
 * On parsing input
 */
object Instruction{
  def apply(line: String): Instruction = line.split(' ') match {
    case Array(s, v) if (s == "nop") => Nop(v.toInt)
    case Array(s, v) if (s == "acc") => Acc(v.toInt)
    case Array(s, v) if (s == "jmp") => Jmp(v.toInt)
    case _ => throw new InputMismatchException(s"Instruction not recognized:\n\t$line")
  }
}