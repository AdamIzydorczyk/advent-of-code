

object Ex3 extends App {
  val report = PuzzleInputDownloader.download("https://adventofcode.com/2021/day/3/input")

  private def partOne = {
    val numbersWithIndexes = report.flatMap(_.split("")
      .map(_.toInt).zipWithIndex)

    val sumsByIndexes = numbersWithIndexes.groupBy(_._2)
      .view.mapValues(_.map(_._1).sum)

    val binaryGamma = sumsByIndexes.toList.sortBy(_._1)
      .map(_._2.toDouble / report.length)
      .map(math.round)
      .map(_.toInt).mkString

    val binaryEpsilon = binaryGamma.map {
      case '1' => true
      case '0' => false
    }.map(!_)
      .map {
        case true => '1'
        case false => '0'
      }.mkString

    val gamma: Int = Integer.parseInt(binaryGamma, 2)
    val epsilon: Int = Integer.parseInt(binaryEpsilon, 2)

    gamma * epsilon
  }
  println(partOne)
}
