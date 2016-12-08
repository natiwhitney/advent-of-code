import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
=======

  val fileName = "dayFour.csv"
  def parseInputs(fileName:String): Array[Int] = {
    println("Reading file input")
    var parsedInputs = new ArrayBuffer[Array[Int]]()
    val bufferedSource = io.Source.fromFile(fileName)
    val sectorIds = new ArrayBuffer[Int]()
    for (line <- bufferedSource.getLines()) {
      val parsedLine = parseLine(line)
      val sectorId = if (checkValid2(parsedLine)) parsedLine._2 else 0
      sectorIds += sectorId
    }
    return sectorIds.toArray
  }

  def checkValid(parsedLine: (String,Int,String)): Boolean = {
    return parsedLine._3 == ((0 to 4) map parsedLine._1).mkString
  }

  def checkValid2(parsedLine: (String,Int,String)): Boolean = {
    if (("north".r findFirstIn parsedLine._1).mkString == "north") {
      println(s"charString:${parsedLine._1}")
      println(s"sectorId:${parsedLine._2}")
      return true
    } else {
      return false
    }
  }

  def parseLine(line: String): (String,Int,String) = {
    var lineSplits = line.split("-")
    val n = lineSplits.length
    val sectorIdCheckSum = lineSplits(n - 1).split('[')
    val sectorId = Integer.parseInt(sectorIdCheckSum(0))
    val checkSum = sectorIdCheckSum(1).split("]")(0)

    val charString = parseChars2(lineSplits,sectorId)
    return (charString, sectorId, checkSum)
  }

  def parseChars2(lineSplits: Array[String], sectorId:Int) : String = {
    val n = lineSplits.length
    val indices = 0 to n-2
    val charStrings = indices map lineSplits
    val chars = new ListBuffer[Char]()
    for (charString <- charStrings) {
      for (c <- charString.toList) {
        chars += rotate(c,sectorId)
      }
    }
    return chars.mkString
  }

  def rotate(c: Char, n: Int): Char = {
    val newInt = ('a' + ((c - 'a') + n) % 26)
    return newInt.toChar
  }

  def parseChars(lineSplits: Array[String]): String = {
    val n = lineSplits.length
    val indices = 0 to n-2
    val charStrings = indices map lineSplits
    val chars = new ListBuffer[Char]()

    for (charString <- charStrings) {
      for (c <- charString.toList) {
        chars += c
      }
    }
    return sortedCharsToString(sortChars(chars.toList))
  }

  def sortChars(chars: List[Char]): List[(Char,Int)] = {
    val charFreqs = chars.groupBy(identity).mapValues(_.size).toList
    val sortedChars = charFreqs.sortWith((x,y) => if (x._2 > y._2) true else if (x._2 < y._2) false else (x._1 < y._1))
    return sortedChars
  }

  def sortedCharsToString(sortedChars: List[(Char,Int)]): String ={
    return sortedChars.map(_._1).mkString
  }

  val result = parseInputs(fileName).reduceLeft(_+_)
  println(s"result:$result")
}
