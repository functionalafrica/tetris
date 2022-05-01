package tetris

import profanity.*
import exoskeleton.*
import gossamer.*
import rudiments.*
import eucalyptus.*
import clairvoyant.*
import turbulence.*

import timekeeping.long

given Log = Log.silent

import unsafeExceptions.canThrowAny

import Keypress.*


case class Block(cells: (Int, Int)*)

//    XX
//  XXXXXX
object Tee extends Block((0, 0), (1, 0), (2, 0), (1, 1))
object Square extends Block((0, 0), (1, 0), (0, 1), (1, 1))
object Straight extends Block((0, 0), (1, 0), (2, 0), (3, 0))

case class GameState(shape: Block, x: Int, y: Int)

object Main extends Daemon:
  def main(using CommandLine): ExitStatus =

    val grid: Array[Array[Boolean]] =
      Array.fill(24)(Array.fill(16)(false))

    val pulsar: LazyList[Keypress] =
      Pulsar()(1000).stream.map { _ => DownArrow }

    def cell(state: GameState, col: Int, row: Int): Boolean =
      grid(row)(col) || state.shape.cells.exists:
        case (dx, dy) => state.x + dx == col && state.y + dy == row

    def render(state: GameState): GameState =
      for row <- 23 to 0 by -1 do
        for col <- 0 until 16 do
          Out.print(if cell(state, col, row) then "▇▇" else "  ")
        Out.println("")
      Out.println(t"\e[25A")
      state

    Tty.capture:
      def events: LazyList[Keypress] =
        Tty.stream[Keypress]
           .multiplexWith(pulsar)
           .takeWhile(_ != Ctrl('C'))


      @annotation.tailrec
      def recur(state: GameState, stream: LazyList[Keypress]): GameState =
        render(state)
        if stream.isEmpty then state
        else stream.head match
          case LeftArrow  => recur(state.copy(x = state.x - 1), stream.tail)
          case RightArrow => recur(state.copy(x = state.x + 1), stream.tail)
          case DownArrow  => recur(state.copy(y = state.y - 1), stream.tail)
          case _          => recur(state, stream.tail)

      val state = recur(GameState(Tee, 8, 22), events)



      Out.println("The final game state was: "+state)

    ExitStatus.Ok








































































object Main2 extends Daemon():
  case class Shape(offsets: (Int, Int)*)
  val Square = Shape((0, 0), (0, 1), (1, 0), (1, 1))
  val Straight = Shape((0, 0), (0, 1), (0, 2), (0, 3))
  val Skew = Shape((0, 0), (0, 1), (1, 1), (1, 2))
  var x = 10
  var y = 10
  var shape = Skew
  val grid: Array[Array[Boolean]] = Array.fill(20)(Array.fill(20)(false))
  
  def cell(col: Int, row: Int) =
    grid(col)(row) || shape.offsets.exists { case (dx, dy) => x + dx == col && y + dy == row }

  def render()(using Stdout): Unit =
    for row <- 0 until 20 do
      for col <- 0 until 20 do
        Out.print(if cell(col, row) then "▇▇" else "  ")
      
      Out.println("")
    
    Out.print(t"\e[20A")

  def main(using CommandLine): ExitStatus =

    val pulsar = Pulsar()(500).stream.map(_ => Keypress.DownArrow)
    Tty.capture:
      Tty.stream[Keypress].multiplexWith(pulsar).foreach:
        keypress =>
          keypress match
            case Keypress.LeftArrow  => x -= 1
            case Keypress.RightArrow => x += 1
            case Keypress.UpArrow    => y -= 1
            case Keypress.DownArrow  => y += 1
            case _          => ()
              
          render()

    ExitStatus.Ok