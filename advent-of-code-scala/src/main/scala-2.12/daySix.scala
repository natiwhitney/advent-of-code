import scala.collection.mutable.{ArrayBuffer, ListBuffer}
/**
  * Created by natashawhitney on 12/5/16.
  */
object daySix extends App {
  val fileName = "daySixInput.csv"

  def parseInputs(fileName:String): Array[Array[Char]] = {
    println("Reading file input")

    val numLines = io.Source.fromFile(fileName).getLines.size
    val numChars = 8
    var parsedInputs = Array.ofDim[Char](numChars,numLines)
    val bufferedSource = io.Source.fromFile(fileName)
    val bufferedLines = bufferedSource.getLines()

    for (i <- 0 to numLines - 1) {
      val line = bufferedLines.next()
      val chars = line.toCharArray
      for (j <- 0 to numChars - 1) {
        parsedInputs(j)(i) = chars(j)
      }
    }
    return parsedInputs
  }

  def mostFreq(list: Array[Char]): Char = {
    return list.groupBy(identity).mapValues(_.size).minBy(_._2)._1
  }

  val parsedInputs = parseInputs(fileName)
  val mostFreqByCol = parsedInputs.map(mostFreq)
  println(s"most freq string:${mostFreqByCol.mkString}")
}

