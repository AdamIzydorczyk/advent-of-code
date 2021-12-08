object Ex4 extends App {
  val bingoData: Seq[String] = PuzzleInputDownloader.download("https://adventofcode.com/2021/day/4/input")
  val boardFieldsIndexes: IndexedSeq[(Int, Int)] = for (counter <- 0 until 5;
                                                        vertical <- 0 until 5;
                                                        horizontal <- 0 until 5) yield (horizontal, vertical)

  val numbers: Array[Int] = bingoData.head.split(",")
    .map(_.toInt)

  val boardsAsText: Iterator[Seq[String]] = bingoData.slice(1, bingoData.length)
    .map(_.split(" ").filterNot(_.isBlank))
    .filterNot(_.isEmpty)
    .flatten
    .sliding(25, 25)

  private def partOne: Int = {
    val boards = boardsAsText
      .map(new Board(_))
      .toList

    for (number <- numbers; board <- boards) {
      board.checkNumberAndCalculateResult(number)
      val result = board.result
      if (result > 0) {
        return number * result
      }
    }
    0
  }

  println(partOne)

  private def partTwo: Int = {
    val boards = boardsAsText
      .map(new Board(_))
      .toList

    var lastScore = 0

    for (number <- numbers; board <- boards) {
      if (board.result == 0) {
        board.checkNumberAndCalculateResult(number)

        if (board.result > 0) {
          lastScore = number * board.result
        }
      }
    }

    lastScore
  }

  println(partTwo)

  class Board(list: Seq[String], var skipped: Boolean = false) {
    private val markedRows: Array[Int] = Array(0, 0, 0, 0, 0)
    private val markedColumns: Array[Int] = Array(0, 0, 0, 0, 0)
    private val fields: Seq[MarkableNumber] = list.zip(boardFieldsIndexes)
      .map(tuple => new MarkableNumber(tuple._1.toInt, tuple._2._1, tuple._2._2))
      .sortBy(_.number)

    var result = 0

    def checkNumberAndCalculateResult(number: Int): Unit = {
      val coordinates = findCoordinates(number)

      if (coordinates.isDefined) {
        val foundNumber = coordinates.get
        foundNumber.marked = true

        markedRows(foundNumber.x) = markedRows(foundNumber.x) + 1
        markedColumns(foundNumber.y) = markedColumns(foundNumber.y) + 1

        val rowsStatus = markedRows(foundNumber.x)
        val columnsStatus = markedColumns(foundNumber.y)

        val COMPLETE_SCORE = 5
        if (rowsStatus == COMPLETE_SCORE || columnsStatus == COMPLETE_SCORE) {
          this.result = fields.filterNot(_.marked)
            .map(_.number)
            .sum
        }
      }
    }

    def findCoordinates(number: Int): Option[MarkableNumber] = {
      for (field <- fields) {
        if (field.number > number) {
          return Option.empty
        } else if (field.number == number) {
          return Option.apply(field)
        }
      }
      Option.empty
    }

    class MarkableNumber(var number: Int, var x: Int, var y: Int, var marked: Boolean = false)

  }

}