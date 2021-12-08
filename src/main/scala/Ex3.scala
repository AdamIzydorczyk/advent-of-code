import scala.annotation.tailrec

object Ex3 extends App {
  val report = PuzzleInputDownloader.download("https://adventofcode.com/2021/day/3/input")

  private def partOne: Int = {
    val numbersWithIndexesByIndexes = report.flatMap(_.split("").zipWithIndex)
      .groupBy(_._2)

    val commonValueOnIndexByIndex = numbersWithIndexesByIndexes.view
      .mapValues(_.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)._1)

    val binaryGamma = commonValueOnIndexByIndex.toList
      .sortBy(_._1)
      .map(_._2._1)
      .mkString

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

  private def partTwo: Int = {
    val bits: Seq[Array[Int]] = report.map(_.split("")
      .map(_.toInt))

    @tailrec
    def calculateRate(commonCondition: (Int, Int) => Int,
                      bits: Seq[Array[Int]],
                      position: Int): Int = {
      if (bits.length <= 1) {
        return Integer.parseInt(bits.head.mkString, 2)
      }

      val bitsOnPosition = bits.map(_ (position))
      val countByBit = bitsOnPosition.groupBy(identity).view
        .mapValues(_.size)
      val zeros = countByBit.getOrElse(0, 0)
      val ones = countByBit.getOrElse(1, 0)
      val common = commonCondition.apply(ones, zeros)

      calculateRate(commonCondition, bits.filter(_ (position) == common), position + 1)
    }

    val o2Rate = calculateRate((ones, zeros) => if (ones >= zeros) 1 else 0, bits, 0)
    val co2Rate = calculateRate((ones, zeros) => if (ones >= zeros) 0 else 1, bits, 0)

    o2Rate * co2Rate
  }

  println(partTwo)

}