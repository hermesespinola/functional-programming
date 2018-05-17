#!/bin/sh
  exec scala "$0" "$@"
!#

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

/**
 * start state: x
 * state -> (see -> [move (<0 or >0), write (0 or 1), movetonode], see -> ...)
 * movetonode == -1 is halt
 **/
object Turing {
  val moves = Array('L', 'R')
  val symbols = Array(0, 1)

  type Direction = Char
  type Symbol = Int
  type Transition = Int
  type SubState = (Direction, Symbol, Transition)
  type State = (SubState, SubState)

  implicit def intToBoolean(x: Int): Boolean = x != 0
  implicit def directionToInt(dir: Direction): Int = if (dir == 'L') -1 else 1

  def printState(state: State) = println(s"""{
    |  0: { move: ${state._1._1}, write: ${state._1._2}, goTo: ${state._1._3} },
    |  1: { move: ${state._2._1}, write: ${state._2._2}, goTo: ${state._2._3} },
    |}
  """.stripMargin)

  type Tape = Array[Int]

    def simulate(machine: Seq[State]) = {
      val tape = Array(0)

      def step(tape: Tape, to: Transition, position: Int, recCount: Int): Option[Tape] =
        if (to == -1) Some(tape)
        else if (recCount > 100) None
        else if (tape(position) == 0) machine(to)._1 match {
          case (newDir, newWrite, newTo) =>
            tape(position) = newWrite
            val newPos = position + directionToInt(newDir)
            val newTape = if (newPos >= tape.length) tape ++ Array(0, 0, 0, 0, 0, 0)
                          else if (newPos < 0) Array(0) ++ tape
                          else tape

            step(newTape, newTo, if (newPos < 0) 0 else newPos, recCount  + 1)
          case _ => None
        }
        else machine(to)._2 match {
          case (newDir, newWrite, newTo) =>
            tape(position) = newWrite
            val newPos = position + directionToInt(newDir)
            val newTape = if (newPos > tape.length) tape ++ Array(0)
                          else if (newPos < 0) Array(0) ++ tape
                          else tape
            // println(s"newTape: ${newTape.toSeq}")
            step(newTape, newTo, if (newPos < 0) 0 else newPos, recCount  + 1)
          case _ => None
        }

        // Always see 0 at first move
        step(tape, machine(0)._1._3, 0, 0)
    }

  def main(args: Array[String]): Unit = {
    // all possible states (all combinations of transitions, see and write)
     val allStates = for {
      move1 <- moves
      move2 <- moves
      write1 <- symbols
      write2 <- symbols
      moveTo1 <- 0 until 4
      moveTo2 <- 0 until 4
      haltTransition <- List(0, 1)
      halt <- List(false, true)
    } yield {
      if (halt)
        if (haltTransition)
          ((move1, write1, -1), (move2, write2, moveTo2))
          else
          ((move1, write1, moveTo1), (move2, write2, -1))
      else
        ((move1, write1, moveTo1), (move2, write2, moveTo2))
    }

    lazy val allTuring = (allStates combinations 4) filter { machine =>
      val noHaltAtStart = machine(0)._1._3 > -1
      val noInfiteLoop1 = machine(0)._1._3 != 0 & machine(0)._2._3 != 0
      val noInfiteLoop2 = machine(1)._1._3 != 1 & machine(1)._2._3 != 1
      val noInfiteLoop3 = machine(2)._1._3 != 2 & machine(2)._2._3 != 2
      val noInfiteLoop4 = machine(3)._1._3 != 3 & machine(3)._2._3 != 3

      noHaltAtStart & noInfiteLoop1 & noInfiteLoop2 & noInfiteLoop3 & noInfiteLoop4
    }

    // The one found in wikipedia
    val perfectBeaber = Seq(
      (('R', 1,  1), ('L', 1, 1)), // 0
      (('L', 1,  0), ('L', 0, 3)), // 1
      (('R', 1,  2), ('R', 0, 0)), // 2
      (('R', 1, -1), ('L', 1, 2))  // 3
    )
    println(perfectBeaber)
    println(simulate(perfectBeaber).get.toList)

    println("Testing machines")
    Future.traverse(allTuring) { machine => Future {
      simulate(machine) match {
        case Some(tape) =>
          val sum = tape.sum
          if (sum >= 7) println(s"${tape.sum} -> ${machine.toSeq}") // A beaber!
          true
        case None => false
      }
    }}
  }
}
