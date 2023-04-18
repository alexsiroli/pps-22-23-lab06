package u06lab.code

import scala.collection.IterableFactory

object Solitaire extends App:

  private type Position = (Int, Int)
  private type Solution = Seq[Position]
  given Int = 0

  private val width = 5
  private val height = 5

  def computeSolutions(width: Int, height: Int)(using n: Int): Seq[Solution] =
    n match
      case 0 => ???

  def render(solution: Solution, width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  computeSolutions(width, height).zipWithIndex foreach ((s, i) =>
    println("\n Soluzione " + (i + 1) + "\n" + render(s, width, height)))