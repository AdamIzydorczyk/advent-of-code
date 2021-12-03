object Ex1 extends App {
  val listOfNumbers: LazyList[Int] = PuzzleInputDownloader.download("https://adventofcode.com/2021/day/1/input")
    .map(_.toInt)

  private def partOne: Int = {
    listOfNumbers
      .sliding(2)
      .count(pair => pair.head < pair.tail.head)
  }
  println(partOne)

  private def partTwo: Int = {
    listOfNumbers
      .sliding(3, 1)
      .map(_.sum)
      .sliding(2)
      .count(pair => pair.head < pair.tail.head)
  }
  println(partTwo)
}
