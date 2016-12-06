import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.immutable.ListMap

/**
  * Created by natashawhitney on 12/5/16.
  */
object dayFour extends App {
  val fileName = "dayFourInput.csv"
  case class charFreq(char: Char, freq: Int) extends Ordered[charFreq] {
    // Required as of Scala 2.11 for reasons unknown - the companion to Ordered
    // should already be in implicit scope
    import scala.math.Ordered.orderingToOrdered

    def compare(that: charFreq): Int = (this.char, this.freq) compare (that.char, that.freq)
  }

  def parseInputs(fileName:String): List[(List[charFreq], Int, String)] = {
    println("Reading file input")

    var parsedInputs = new ListBuffer[(List[charFreq], Int, String)]()
    val bufferedSource = io.Source.fromFile(fileName)

    for (line <- bufferedSource.getLines()) {
      val parsedLine = line.split("-")
      val n = parsedLine.length
      val sectorIdCheckSum = parsedLine(n - 1)
      val parsedSICS = sectorIdCheckSum.split('[')
      val sectorId = Integer.parseInt(parsedSICS(0))
      val checkSum = parsedSICS(1).split(']')(0)

      val lowercaseStrings = 0 to (n - 2) map parsedLine
      var letters = new ListBuffer[List[Char]]()
      for (lowercaseString <- lowercaseStrings) {
        var chars = lowercaseString.toList
        letters += chars
      }

    }
    return parsedInputs.toList
  }

  def sortCharFreqs(charFreqs:List[charFreq]): List[charFreq] = {
    charFreqs.sortBy(cf => (cf.char, cf.freq))
  }

  def generateCheckSum(charFreqs:List[charFreq]): String = {
    sortCharFreqs(charFreqs).map(cf => cf.char).mkString
  }

  def validCheckSumP(parsedLine:(List[charFreq], Int, String)): Boolean = {
    val checkSum = generateCheckSum(parsedLine._1).slice(0,5)
    if (checkSum == parsedLine._3) {
      println(s"valid check sum: $checkSum")
      return true
    } else {
      println(s"invalid check sum: $parsedLine._3")
      println(s"generated check sum: $checkSum")
      return false
    }
  }

  def inputs = parseInputs(fileName)
  var sectorIdSum = 0
  for (input <- inputs) {
    if (validCheckSumP(input)) sectorIdSum += input._2
  }
  println(s"sector id sum: $sectorIdSum")
}