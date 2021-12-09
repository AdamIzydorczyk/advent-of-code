import java.lang.Math.{abs, max, min}

object Ex5 extends App {
  private val coordinates = PuzzleInputDownloader.download("https://adventofcode.com/2021/day/5/input")
    .map("\\d+".r.findAllMatchIn(_)
      .map(_.matched)
      .map(_.toInt))
    .map(_.toArray)
    .map(arr => (arr(0), arr(1), arr(2), arr(3)))

  private def partOne = {
    val diagram = Array.ofDim[Int](1000, 1000)

    coordinates
      .filter(c => c._1 == c._3 || c._2 == c._4).foreach(coordinates => {
      for (posX <- min(coordinates._1, coordinates._3) to max(coordinates._1, coordinates._3);
           posY <- min(coordinates._2, coordinates._4) to max(coordinates._2, coordinates._4))
        diagram(posY)(posX) = diagram(posY)(posX) + 1
    })

    diagram.flatten.count(_ >= 2)
  }

  println(partOne)

  private def partTwo = {
    val diagram = Array.ofDim[Int](1000, 1000)

    coordinates.foreach(coordinates => {
      if (coordinates._1 == coordinates._3 || coordinates._2 == coordinates._4) {
        for (posX <- min(coordinates._1, coordinates._3) to max(coordinates._1, coordinates._3);
             posY <- min(coordinates._2, coordinates._4) to max(coordinates._2, coordinates._4))
          diagram(posY)(posX) = diagram(posY)(posX) + 1
      }

      val xDiff = coordinates._1 - coordinates._3
      val yDiff = coordinates._2 - coordinates._4

      val minX = min(coordinates._1, coordinates._3)
      val maxX = max(coordinates._1, coordinates._3)
      val minY = min(coordinates._2, coordinates._4)
      val maxY = max(coordinates._2, coordinates._4)

      if (xDiff == yDiff) {
        for (posX <- minX to maxX;
             posY <- minY to maxY)
          if (maxX - posX == maxY - posY) {
            diagram(posY)(posX) = diagram(posY)(posX) + 1
          }
      } else if (abs(xDiff) == abs(yDiff)) {
        for (posX <- minX to maxX;
             posY <- minY to maxY)
          if (maxX - posX == posY - minY) {
            diagram(posY)(posX) = diagram(posY)(posX) + 1
          }
      }
    })

    diagram.flatten.count(_ >= 2)
  }

  println(partTwo)
}