import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by natashawhitney on 12/5/16.
  */
object dayFour extends App {

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
