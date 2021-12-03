import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import scala.jdk.StreamConverters._

object Ex2 extends App {
  private val moves = PuzzleInputDownloader.download("https://adventofcode.com/2021/day/2/input")
  private val movesAndNumbers = moves.map(_.split(" "))
    .map(movesAndNumbers => (movesAndNumbers(0), movesAndNumbers(1).toInt))

  def partOne: Int = {
    val amountByMoves = movesAndNumbers.groupBy(_._1)
      .view.mapValues(value => value.map(_._2).sum)

    val ups: Int = amountByMoves.getOrElse("up", 0)
    val downs: Int = amountByMoves.getOrElse("down", 0)
    val forwards: Int = amountByMoves.getOrElse("forward", 0)

    (downs - ups) * forwards
  }
  println(partOne)

  private def partTwo: Int = {
    var horizontal = 0
    var depth = 0
    var aim = 0

    movesAndNumbers.foreach {
      case moveAndNumber@("up", _) => aim -= moveAndNumber._2;
      case moveAndNumber@("down", _) => aim += moveAndNumber._2;
      case moveAndNumber@("forward", _) => horizontal += moveAndNumber._2; depth += (aim * moveAndNumber._2)
    }

    horizontal * depth
  }
  println(partTwo)
}
